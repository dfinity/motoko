use motoko_rts_macros::ic_mem_fn;

use crate::{
    memory::Memory,
    types::{
        is_skewed, mark, object_size, Bytes, Obj, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN,
        TAG_FREE_BLOCK_MIN, TAG_NULL, TAG_OBJECT, TAG_ONE_WORD_FILLER,
    },
    visitor::visit_pointer_fields,
};

use self::{
    free_list::{FreeBlock, SegregatedFreeList},
    mark_stack::MarkStack,
};

pub mod free_list;
pub mod mark_stack;
#[cfg(debug_assertions)]
pub mod sanity_checks;
pub mod write_barrier;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(_mem: &mut M) {
    crate::memory::ic::initialize_memory(true);
    IncrementalGC::<M>::initialize();
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    if !IncrementalGC::<M>::pausing() || should_start() {
        incremental_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    let old_free_size = FREE_LIST.as_ref().unwrap().total_size();
    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *crate::continuation_table::continuation_table_loc(),
    };
    IncrementalGC::instance(mem).empty_call_stack_increment(limits, roots);
    update_statistics::<M>(old_free_size);
}

#[cfg(feature = "ic")]
static mut LAST_ALLOCATED: Bytes<u64> = Bytes(0);

#[cfg(feature = "ic")]
unsafe fn should_start() -> bool {
    use crate::memory::ic;
    debug_assert!(ic::ALLOCATED >= LAST_ALLOCATED);
    const THRESHOLD: Bytes<u64> = Bytes(32 * 1024 * 1024);
    ic::ALLOCATED - LAST_ALLOCATED >= THRESHOLD
}

#[cfg(feature = "ic")]
unsafe fn update_statistics<M: Memory>(old_free_size: Bytes<u32>) {
    use crate::memory::ic;
    let free_size = FREE_LIST.as_ref().unwrap().total_size();
    if free_size > old_free_size {
        ic::RECLAIMED += Bytes(u64::from((free_size - old_free_size).as_u32()));
    }
    if IncrementalGC::<M>::pausing() {
        LAST_ALLOCATED = ic::ALLOCATED;
        let free_size = FREE_LIST.as_ref().unwrap().total_size().as_u32();
        debug_assert!(free_size <= ic::HP);
        let live_set = Bytes(ic::HP - free_size);
        ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_set);
    }
}

enum Phase {
    Pause,
    Mark(MarkState),
    Sweep(SweepState),
    Stop, // On canister upgrade
}

struct MarkState {
    heap_base: usize,
    mark_stack: MarkStack,
    complete: bool,
}

struct SweepState {
    sweep_line: usize,
    heap_end: usize,
}

/// GC state retained over multiple GC increments.
static mut PHASE: Phase = Phase::Pause;

/// Free list activated on incremental GC.
pub(crate) static mut FREE_LIST: Option<SegregatedFreeList> = None;

/// Heap limits.
pub struct Limits {
    pub base: usize,
    pub free: usize,
}

/// GC Root set.
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `IncrementalGC::mark_roots()`.
}

/// Incremental GC.
/// Each GC call has its new `Memory` instance that shares the common GC state `PHASE`.
pub struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
}

impl<'a, M: Memory + 'a> IncrementalGC<'a, M> {
    const LARGE_INCREMENT_LIMIT: usize = 32 * 1024;
    const SMALL_INCREMENT_LIMIT: usize = 256;

    /// (Re-)Initialize the entire incremental garbage collector.
    /// Called on a runtime system start with incremental GC and also during RTS testing.
    pub unsafe fn initialize() {
        PHASE = Phase::Pause;
        FREE_LIST = Some(SegregatedFreeList::new());
    }

    pub fn instance(mem: &'a mut M) -> IncrementalGC<'a, M> {
        IncrementalGC { mem }
    }

    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * In future: The moving phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(&mut self, limits: Limits, roots: Roots) {
        if Self::pausing() {
            self.start_marking(limits.base, roots);
        } else if Self::mark_completed() {
            self.start_sweeping(limits.base, limits.free);
        } else {
            self.increment(Self::LARGE_INCREMENT_LIMIT);
        }
    }

    /// Pre-update field-level write barrier peforming snapshot-at-the-beginning marking.
    /// The barrier is only effective while the GC is in the mark phase.
    /// A GC increment is implicitly combined with the write barrier.
    #[inline]
    unsafe fn pre_write_barrier(&mut self, value: Value) {
        if value.is_ptr() {
            if let Phase::Mark(state) = &mut PHASE {
                if value.get_ptr() >= state.heap_base && !state.complete {
                    let mut gc = Increment::instance(self.mem, state, Self::SMALL_INCREMENT_LIMIT);
                    gc.mark_object(value);
                    gc.mark_phase_increment();
                }
            }
        }
    }

    /// Large GC increment invoked before a mutator heap allocation.
    /// No recursive increments if GC allocates heap space for the mark stack.
    #[inline]
    pub unsafe fn allocation_increment(&mut self) {
        self.increment(Self::LARGE_INCREMENT_LIMIT);
    }

    unsafe fn pausing() -> bool {
        match &PHASE {
            Phase::Pause => true,
            _ => false,
        }
    }

    unsafe fn increment(&mut self, increment_limit: usize) {
        match &mut PHASE {
            Phase::Pause | Phase::Stop => {}
            Phase::Mark(state) => {
                Increment::instance(self.mem, state, increment_limit).mark_phase_increment()
            }
            Phase::Sweep(state) => {
                Increment::instance(self.mem, state, increment_limit).sweep_phase_increment()
            }
        }
    }

    unsafe fn start_marking(&mut self, heap_base: usize, roots: Roots) {
        debug_assert!(Self::pausing());

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(false);

        let mark_stack = MarkStack::new(self.mem);
        let state = MarkState {
            heap_base,
            mark_stack,
            complete: false,
        };
        PHASE = Phase::Mark(state);
        if let Phase::Mark(state) = &mut PHASE {
            let mut gc = Increment::instance(self.mem, state, Self::LARGE_INCREMENT_LIMIT);
            gc.mark_roots(roots);
            gc.mark_phase_increment();
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn mark_completed() -> bool {
        match &PHASE {
            Phase::Mark(state) => {
                debug_assert!(!state.complete || state.mark_stack.is_empty());
                state.complete
            }
            _ => false,
        }
    }

    unsafe fn start_sweeping(&mut self, heap_base: usize, heap_end: usize) {
        debug_assert!(Self::mark_completed());
        let state = SweepState {
            sweep_line: heap_base,
            heap_end,
        };
        PHASE = Phase::Sweep(state);

        #[cfg(debug_assertions)]
        FREE_LIST.as_ref().unwrap().sanity_check();

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(true);

        self.increment(Self::LARGE_INCREMENT_LIMIT);
    }
}

/// GC increment.
struct Increment<'a, M: Memory, S: 'static> {
    mem: &'a mut M,
    state: &'static mut S,
    steps: usize,
    increment_limit: usize,
}

impl<'a, M: Memory + 'a, S: 'static> Increment<'a, M, S> {
    fn instance(
        mem: &'a mut M,
        state: &'static mut S,
        increment_limit: usize,
    ) -> Increment<'a, M, S> {
        Increment {
            mem,
            state,
            steps: 0,
            increment_limit,
        }
    }
}

impl<'a, M: Memory + 'a> Increment<'a, M, MarkState> {
    unsafe fn mark_roots(&mut self, roots: Roots) {
        self.mark_static_roots(roots.static_roots);
        self.mark_continuation_table(roots.continuation_table);
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            debug_assert!((mutbox as usize) < self.state.heap_base);
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.state.heap_base {
                self.mark_object(value);
            }
            self.steps += 1;
        }
    }

    unsafe fn mark_continuation_table(&mut self, continuation_table: Value) {
        if continuation_table.is_ptr() {
            self.mark_object(continuation_table);
        }
    }

    unsafe fn mark_object(&mut self, value: Value) {
        self.steps += 1;
        debug_assert!(!self.state.complete);
        debug_assert!((value.get_ptr() >= self.state.heap_base));
        let object = value.as_obj();
        if object.is_marked() {
            return;
        }
        object.mark();
        debug_assert!(object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL);
        self.state.mark_stack.push(self.mem, value);
    }

    unsafe fn mark_phase_increment(&mut self) {
        if self.state.complete {
            // allocation after complete marking, wait until next empty call stack increment
            debug_assert!(self.state.mark_stack.is_empty());
            return;
        }
        while let Some(value) = self.state.mark_stack.pop() {
            debug_assert!(value.is_ptr());
            debug_assert!(value.as_obj().is_marked());
            self.mark_fields(value.as_obj());

            self.steps += 1;
            if self.steps > self.increment_limit {
                return;
            }
        }
        self.complete_marking();
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        debug_assert!(object.is_marked());
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.state.heap_base,
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                debug_assert!((array as *mut Obj).is_marked());
                const SLICE_INCREMENT: u32 = 127;
                debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    (*array).header.raw_tag = mark(new_start);
                    debug_assert!(!gc.state.complete);
                    gc.state
                        .mark_stack
                        .push(gc.mem, Value::from_ptr(array as usize));
                    debug_assert!((array as *mut Obj).is_marked());
                    gc.steps += SLICE_INCREMENT as usize;
                    new_start
                } else {
                    (*array).header.raw_tag = mark(TAG_ARRAY);
                    debug_assert!((array as *mut Obj).is_marked());
                    gc.steps += array.len() as usize;
                    array.len()
                }
            },
        );
    }

    unsafe fn complete_marking(&mut self) {
        debug_assert!(!self.state.complete);
        self.state.complete = true;

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_mark_completeness(self.mem);

        #[cfg(debug_assertions)]
        self.state.mark_stack.assert_is_garbage();
    }
}

impl<'a, M: Memory + 'a> Increment<'a, M, SweepState> {
    unsafe fn sweep_phase_increment(&mut self) {
        while self.state.sweep_line < self.state.heap_end {
            let free_space = self.collect_contiguous_free_space();
            if free_space > 0 {
                FREE_LIST
                    .as_mut()
                    .unwrap()
                    .free_space(self.state.sweep_line, Bytes(free_space as u32));

                if self.steps > self.increment_limit {
                    // Continue merging with this free segment on the next GC increment.
                    return;
                }
                self.state.sweep_line += free_space;
            } else {
                let object = self.state.sweep_line as *mut Obj;
                debug_assert!(object.tag() >= TAG_OBJECT && object.tag() <= TAG_NULL);
                debug_assert!(object.is_marked());
                object.unmark();
                let size = object_size(self.state.sweep_line).to_bytes().as_usize();
                self.state.sweep_line += size;
            }
            debug_assert!(self.state.sweep_line <= self.state.heap_end);

            self.steps += 1;
            if self.steps > self.increment_limit {
                return;
            }
        }
        self.complete_sweeping();
    }

    unsafe fn complete_sweeping(&mut self) {
        PHASE = Phase::Pause;

        #[cfg(debug_assertions)]
        FREE_LIST.as_ref().unwrap().sanity_check();

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(false);
    }

    unsafe fn collect_contiguous_free_space(&mut self) -> usize {
        let mut address = self.state.sweep_line;
        while address < self.state.heap_end && !(address as *mut Obj).is_marked() {
            let tag = (address as *mut Obj).tag();
            if tag >= TAG_FREE_BLOCK_MIN {
                let block = address as *mut FreeBlock;
                address += block.size().as_usize();
            } else {
                debug_assert!(tag >= TAG_OBJECT && tag <= TAG_ONE_WORD_FILLER);
                address += object_size(address).to_bytes().as_usize();
            }
            debug_assert!(address <= self.state.heap_end);

            self.steps += 1;
            if self.steps > self.increment_limit {
                break;
            }
        }
        return address - self.state.sweep_line;
    }
}

/// Incremental GC allocation scheme:
/// * During pause phase:
///   - New objects are not marked. Can be reclaimed on the next GC run.
/// * During mark phase:
///   - New allocated objects are conservatively marked and cannot be reclaimed in the
///     current GC run. This is necessary because the incremental GC does neither scan
///     nor use write barriers on the call stack.
/// * During sweep phase:
///   - If allocated below the sweep line, new allocations should not be marked as the
///     current GC run will no longer visit this object and not reset the mark.
///   - If allocated at or above the sweep line, new allocations are conservatively marked
///     to retain them for the next GC run.
/// Summary: New allocated objects are conservatively retained during an active GC run.
/// `new_object` is the unskewed object pointer.
/// Also import for compiler-generated code to situatively set the mark bit for new heap allocations.
#[no_mangle]
pub unsafe extern "C" fn mark_new_allocation(new_object: *mut Obj) {
    debug_assert!(!is_skewed(new_object as u32));
    let should_mark = match &PHASE {
        Phase::Pause | Phase::Stop => false,
        Phase::Mark(_) => true,
        Phase::Sweep(state) => new_object as usize >= state.sweep_line,
    };
    if should_mark {
        debug_assert!(!new_object.is_marked());
        new_object.mark();
    }
}

/// GC increment triggered by the mutator on a heap allocation.
#[ic_mem_fn(ic_only)]
unsafe fn allocation_increment<M: Memory>(mem: &mut M) {
    IncrementalGC::instance(mem).allocation_increment();
}

/// Stop the GC before performing upgrade. Otherwise, GC increments
/// on allocation and writes may interfere with the upgrade mechanism
/// that invalidates object tags during stream serialization.
#[no_mangle]
pub unsafe extern "C" fn stop_gc_on_upgrade() {
    PHASE = Phase::Stop;
}
