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

#[cfg(feature = "ic")]
static mut LAST_ALLOCATED: Bytes<u64> = Bytes(0);

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    if !IncrementalGC::<M>::pausing() || should_start() {
        incremental_gc(mem);
        if IncrementalGC::<M>::pausing() {
            update_statistics();
        }
    }
}

#[cfg(feature = "ic")]
unsafe fn should_start() -> bool {
    use crate::memory::ic;
    assert!(ic::ALLOCATED >= LAST_ALLOCATED);
    const THRESHOLD: Bytes<u64> = Bytes(32 * 1024 * 1024);
    ic::ALLOCATED - LAST_ALLOCATED >= THRESHOLD
}

#[cfg(feature = "ic")]
unsafe fn update_statistics() {
    use crate::memory::ic;
    LAST_ALLOCATED = ic::ALLOCATED;
    let free_size = FREE_LIST.as_ref().unwrap().total_size().as_u32();
    assert!(free_size <= ic::HP);
    let live_set = Bytes(ic::HP - free_size);
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_set);
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
    IncrementalGC::empty_call_stack_increment(mem, limits, roots);
    let free_size = FREE_LIST.as_ref().unwrap().total_size();
    if free_size > old_free_size {
        ic::RECLAIMED += Bytes(u64::from((free_size - old_free_size).as_u32()));
    }
}

enum Phase {
    Pause,
    Mark(MarkState),
    Sweep(SweepState),
}

struct MarkState {
    pub heap_base: usize,
    pub mark_stack: MarkStack,
    pub complete: bool,
}

struct SweepState {
    pub sweep_line: usize,
    pub heap_end: usize,
}

static mut PHASE: Phase = Phase::Pause;

/// Heap limits
pub struct Limits {
    pub base: usize,
    pub free: usize,
}

/// GC Root set
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `mark_roots()`.
}

pub(crate) static mut FREE_LIST: Option<SegregatedFreeList> = None;

/// Incremental GC.
/// Each increment call can have its new instance that shares the common GC state `PHASE`.
pub struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
    steps: usize,
    increment_limit: usize,
}

impl<'a, M: Memory + 'a> IncrementalGC<'a, M> {
    const LARGE_INCREMENT_LIMIT: usize = 256 * 1024;
    const SMALL_INCREMENT_LIMIT: usize = 256;

    pub unsafe fn reset_for_testing() {
        PHASE = Phase::Pause;
        FREE_LIST = Some(SegregatedFreeList::new());
    }

    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * The moving phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(mem: &'a mut M, limits: Limits, roots: Roots) {
        let mut gc = Self::instance(mem, Self::LARGE_INCREMENT_LIMIT);
        if Self::pausing() {
            gc.start_marking(limits.base, roots);
        }
        if Self::mark_completed() {
            gc.start_sweeping(limits.base, limits.free);
        }
        gc.increment();
    }

    /// Pre-update field-level write barrier peforming snapshot-at-the-beginning marking.
    /// The barrier is only effective while the GC is in the mark phase.
    /// A GC increment is implicitly combined with the write barrier.
    #[inline]
    pub unsafe fn pre_write_barrier(mem: &'a mut M, value: Value) {
        if value.is_ptr() {
            if let Phase::Mark(state) = &PHASE {
                if value.get_ptr() >= state.heap_base {
                    let mut gc = Self::instance(mem, Self::SMALL_INCREMENT_LIMIT);
                    if !state.complete {
                        gc.mark_object(value);
                        gc.increment();
                    }
                }
            }
        }
    }

    /// Large GC increment invoked before a mutator heap allocation.
    /// No recursive increments if GC allocates heap space for the mark stack.
    pub unsafe fn allocation_increment(mem: &'a mut M) {
        Self::instance(mem, Self::LARGE_INCREMENT_LIMIT).increment();
    }

    unsafe fn pausing() -> bool {
        match &PHASE {
            Phase::Pause => true,
            _ => false,
        }
    }

    unsafe fn mark_completed() -> bool {
        match &PHASE {
            Phase::Mark(state) => {
                assert!(!state.complete || state.mark_stack.is_empty());
                state.complete
            }
            _ => false,
        }
    }

    fn instance(mem: &'a mut M, increment_limit: usize) -> IncrementalGC<'a, M> {
        IncrementalGC {
            mem,
            steps: 0,
            increment_limit,
        }
    }

    unsafe fn increment(&mut self) {
        match &mut PHASE {
            Phase::Pause => {}
            Phase::Mark(_) => self.mark_phase_increment(),
            Phase::Sweep(_) => self.sweep_phase_increment(),
        }
    }

    unsafe fn start_marking(&mut self, heap_base: usize, roots: Roots) {
        assert!(Self::pausing());

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
        self.mark_roots(roots);
    }

    unsafe fn mark_roots(&mut self, roots: Roots) {
        self.mark_static_roots(roots.static_roots);
        self.mark_continuation_table(roots.continuation_table);
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            assert!((mutbox as usize) < Self::heap_base());
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= Self::heap_base() {
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
        if let Phase::Mark(state) = &mut PHASE {
            self.steps += 1;
            assert!(!state.complete);
            assert!((value.get_ptr() >= state.heap_base));
            let object = value.as_obj();
            if object.is_marked() {
                return;
            }
            object.mark();
            assert!(
                object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL
            );
            state.mark_stack.push(self.mem, value);
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn mark_phase_increment(&mut self) {
        if let Phase::Mark(state) = &mut PHASE {
            if state.complete {
                // allocation after complete marking, wait until next empty call stack increment
                assert!(state.mark_stack.is_empty());
                return;
            }
            while let Some(value) = state.mark_stack.pop() {
                assert!(value.is_ptr());
                assert!(value.as_obj().is_marked());
                self.mark_fields(value.as_obj());

                self.steps += 1;
                if self.steps > self.increment_limit {
                    return;
                }
            }
            self.complete_marking();
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn complete_marking(&mut self) {
        if let Phase::Mark(state) = &mut PHASE {
            assert!(!state.complete);
            state.complete = true;

            #[cfg(debug_assertions)]
            #[cfg(feature = "ic")]
            sanity_checks::check_mark_completeness(self.mem);
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        assert!(object.is_marked());
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            Self::heap_base(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                assert!((array as *mut Obj).is_marked());
                const SLICE_INCREMENT: u32 = 127;
                assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    (*array).header.raw_tag = mark(new_start);
                    if let Phase::Mark(state) = &mut PHASE {
                        assert!(!state.complete);
                        state
                            .mark_stack
                            .push(gc.mem, Value::from_ptr(array as usize));
                    } else {
                        panic!("Invalid phase");
                    }
                    assert!((array as *mut Obj).is_marked());
                    gc.steps += SLICE_INCREMENT as usize;
                    new_start
                } else {
                    (*array).header.raw_tag = mark(TAG_ARRAY);
                    assert!((array as *mut Obj).is_marked());
                    gc.steps += array.len() as usize;
                    array.len()
                }
            },
        );
    }

    unsafe fn start_sweeping(&mut self, heap_base: usize, heap_end: usize) {
        assert!(Self::mark_completed());
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
    }

    unsafe fn sweep_phase_increment(&mut self) {
        if let Phase::Sweep(state) = &mut PHASE {
            while state.sweep_line < state.heap_end {
                let free_space = self.contiguous_free_space(state.sweep_line, state.heap_end);
                if free_space > 0 {
                    FREE_LIST
                        .as_mut()
                        .unwrap()
                        .free_space(state.sweep_line, Bytes(free_space as u32));

                    if self.steps > self.increment_limit {
                        // Continue merging with this free segment on the next GC increment.
                        return;
                    }
                    state.sweep_line += free_space;
                } else {
                    let object = state.sweep_line as *mut Obj;
                    assert!(object.tag() >= TAG_OBJECT && object.tag() <= TAG_NULL);
                    assert!(object.is_marked());
                    object.unmark();
                    state.sweep_line += object_size(state.sweep_line).to_bytes().as_usize();
                }
                assert!(state.sweep_line <= state.heap_end);

                self.steps += 1;
                if self.steps > self.increment_limit {
                    return;
                }
            }
            self.complete_sweeping();
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn complete_sweeping(&mut self) {
        if let Phase::Sweep(_) = &mut PHASE {
            PHASE = Phase::Pause;

            #[cfg(debug_assertions)]
            FREE_LIST.as_ref().unwrap().sanity_check();

            #[cfg(debug_assertions)]
            #[cfg(feature = "ic")]
            sanity_checks::check_memory(false);
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn contiguous_free_space(&mut self, start_address: usize, end_address: usize) -> usize {
        let mut address = start_address;
        while address < end_address && !(address as *mut Obj).is_marked() {
            let tag = (address as *mut Obj).tag();
            if tag >= TAG_FREE_BLOCK_MIN {
                let block = address as *mut FreeBlock;
                address += block.size().as_usize();
            } else {
                assert!(tag >= TAG_OBJECT && tag <= TAG_ONE_WORD_FILLER);
                address += object_size(address).to_bytes().as_usize();
            }
            assert!(address <= end_address);

            self.steps += 1;
            if self.steps > self.increment_limit {
                break;
            }
        }
        return address - start_address;
    }

    unsafe fn heap_base() -> usize {
        match &PHASE {
            Phase::Mark(state) => state.heap_base,
            _ => panic!("Invalid phase"),
        }
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
pub unsafe fn mark_new_allocation(new_object: *mut Obj) {
    assert!(!is_skewed(new_object as u32));
    let should_mark = match &mut PHASE {
        Phase::Pause => false,
        Phase::Mark(_) => true,
        Phase::Sweep(state) => new_object as usize >= state.sweep_line,
    };
    if should_mark {
        assert!(!new_object.is_marked());
        new_object.mark();
    }
}
