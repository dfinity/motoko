use motoko_rts_macros::ic_mem_fn;

use crate::{mem_utils::memcpy_words, memory::Memory, types::*, visitor::visit_pointer_fields};

use self::{
    mark_stack::MarkStack,
    partition_map::{Partition, PartitionMap, MAX_PARTITIONS},
};

pub mod mark_stack;
pub mod partition_map;
#[cfg(debug_assertions)]
pub mod sanity_checks;
pub mod write_barrier;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(_mem: &mut M) {
    use crate::memory::ic;
    ic::initialize_memory(true);
    IncrementalGC::<M>::initialize(ic::get_aligned_heap_base() as usize);
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
    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *crate::continuation_table::continuation_table_loc(),
    };
    record_increment_start::<M>();
    IncrementalGC::instance(mem).empty_call_stack_increment(limits, roots);
    record_increment_stop::<M>();
}

#[cfg(feature = "ic")]
static mut LAST_HEAP_OCCUPATION: usize = 0;

#[cfg(feature = "ic")]
unsafe fn should_start() -> bool {
    use crate::gc::incremental::partition_map::PARTITION_SIZE;
    const RELATIVE_GROWTH_THRESHOLD: f64 = 0.75;
    const CRITICAL_LIMIT: usize = usize::MAX - 256 * 1024 * 1024;
    let occupation = heap_occupation();
    debug_assert!(occupation >= LAST_HEAP_OCCUPATION);
    let absolute_growth = occupation - LAST_HEAP_OCCUPATION;
    let relative_growth = absolute_growth as f64 / occupation as f64;
    relative_growth > RELATIVE_GROWTH_THRESHOLD && absolute_growth >= PARTITION_SIZE
        || occupation >= CRITICAL_LIMIT
}

#[cfg(feature = "ic")]
unsafe fn record_increment_start<M: Memory>() {
    if IncrementalGC::<M>::pausing() {
        LAST_HEAP_OCCUPATION = heap_occupation();
    }
}

#[cfg(feature = "ic")]
unsafe fn heap_occupation() -> usize {
    use crate::memory::ic;
    PARTITION_MAP
        .as_ref()
        .unwrap()
        .occupied_size(ic::HP as usize)
        .as_usize()
}

#[cfg(feature = "ic")]
unsafe fn record_increment_stop<M: Memory>() {
    use crate::memory::ic;
    if IncrementalGC::<M>::pausing() {
        let occupation = PARTITION_MAP
            .as_ref()
            .unwrap()
            .occupied_size(ic::HP as usize);
        ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, occupation);
    }
}

enum Phase {
    Pause,
    Mark(MarkState),
    Evacuate(EvacuationState),
    Stop, // On canister upgrade
}

struct MarkState {
    heap_base: usize,
    mark_stack: MarkStack,
    complete: bool,
}

struct EvacuationState {
    partition_index: usize,
    sweep_address: Option<usize>,
}

/// GC state retained over multiple GC increments.
static mut PHASE: Phase = Phase::Pause;
pub static mut PARTITION_MAP: Option<PartitionMap> = None;

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
    /// (Re-)Initialize the entire incremental garbage collector.
    /// Called on a runtime system start with incremental GC and also during RTS testing.
    pub unsafe fn initialize(heap_base: usize) {
        PHASE = Phase::Pause;
        PARTITION_MAP = Some(PartitionMap::new(heap_base));
    }

    pub unsafe fn instance(mem: &'a mut M) -> IncrementalGC<'a, M> {
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
            self.start_evacuating();
        } else if Self::evacuation_completed() {
            println!(100, "EVACUATION COMPLETED");
        } else {
            self.increment();
        }
    }

    /// Pre-update field-level write barrier peforming snapshot-at-the-beginning marking.
    /// The barrier is only effective while the GC is in the mark phase.
    #[inline]
    unsafe fn pre_write_barrier(&mut self, value: Value) {
        if let Phase::Mark(state) = &mut PHASE {
            if value.points_to_or_beyond(state.heap_base) && !state.complete {
                let mut gc = Increment::instance(self.mem, state);
                gc.mark_object(value);
            }
        }
    }

    unsafe fn pausing() -> bool {
        match &PHASE {
            Phase::Pause => true,
            _ => false,
        }
    }

    unsafe fn increment(&mut self) {
        match &mut PHASE {
            Phase::Pause | Phase::Stop => {}
            Phase::Mark(state) => Increment::instance(self.mem, state).mark_phase_increment(),
            Phase::Evacuate(state) => {
                Increment::instance(self.mem, state).evacuation_phase_increment()
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
            let mut gc = Increment::instance(self.mem, state);
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

    unsafe fn start_evacuating(&mut self) {
        let state = EvacuationState {
            partition_index: 0,
            sweep_address: None,
        };
        PHASE = Phase::Evacuate(state);
        if let Phase::Evacuate(state) = &mut PHASE {
            let mut gc = Increment::instance(self.mem, state);
            gc.initiate_evacuations();
            gc.evacuation_phase_increment();
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn evacuation_completed() -> bool {
        match &PHASE {
            Phase::Evacuate(state) => state.partition_index == MAX_PARTITIONS,
            _ => false,
        }
    }
}

/// GC increment.
struct Increment<'a, M: Memory, S: 'static> {
    mem: &'a mut M,
    partition_map: &'a mut PartitionMap,
    state: &'static mut S,
    steps: usize,
}

impl<'a, M: Memory + 'a, S: 'static> Increment<'a, M, S> {
    unsafe fn instance(mem: &'a mut M, state: &'static mut S) -> Increment<'a, M, S> {
        Increment {
            mem,
            partition_map: PARTITION_MAP.as_mut().unwrap(),
            state,
            steps: 0,
        }
    }

    const INCREMENT_LIMIT: usize = 500_000;
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
        assert!(!value.is_forwarded());
        let object = value.as_obj();
        if object.is_marked() {
            return;
        }
        object.mark();
        self.partition_map.record_marked_space(object);
        debug_assert!(
            object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL
        );
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
            if self.steps > Self::INCREMENT_LIMIT {
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
                const SLICE_INCREMENT: u32 = 128;
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
                    gc.steps += (array.len() % SLICE_INCREMENT) as usize;
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

impl<'a, M: Memory + 'a> Increment<'a, M, EvacuationState> {
    unsafe fn initiate_evacuations(&mut self) {
        self.partition_map.plan_evacuations();
    }

    unsafe fn evacuation_phase_increment(&mut self) {
        while self.state.partition_index < MAX_PARTITIONS {
            if self.current_partition().to_be_evacuated() {
                if self.state.sweep_address.is_none() {
                    self.state.sweep_address = Some(self.current_partition().evacuation_start());
                }
                self.evacuate_partition();
                if self.steps > Self::INCREMENT_LIMIT {
                    return;
                }
            }
            self.state.partition_index += 1;
            self.state.sweep_address = None;
        }
    }

    unsafe fn current_partition(&mut self) -> &Partition {
        self.partition_map.get_partition(self.state.partition_index)
    }

    unsafe fn evacuate_partition(&mut self) {
        let end_address = self.current_partition().end_address();
        while self.state.sweep_address.unwrap() < end_address {
            let raw_tag = *(self.state.sweep_address.unwrap() as *mut Tag);
            if is_marked(raw_tag) {
                let original = self.state.sweep_address.unwrap() as *mut Obj;
                self.evacuate_object(original);
            }
            let size = block_size(self.state.sweep_address.unwrap());
            *self.state.sweep_address.as_mut().unwrap() += size.to_bytes().as_usize();
            assert!(self.state.sweep_address.unwrap() <= end_address);
            self.steps += 1;
            if self.steps > Self::INCREMENT_LIMIT {
                return;
            }
        }
    }

    unsafe fn evacuate_object(&mut self, original: *mut Obj) {
        debug_assert!(original.tag() >= TAG_OBJECT && original.tag() <= TAG_NULL);
        assert!(!original.is_forwarded());
        assert!(original.is_marked());
        let size = block_size(original as usize);
        let new_address = self.mem.alloc_words(size);
        let copy = new_address.get_ptr() as *mut Obj;
        memcpy_words(copy as usize, original as usize, size);
        (*copy).forward = new_address;
        (*original).forward = new_address;
        assert!(!copy.is_forwarded());
        assert!(original.is_forwarded());
    }
}

/// Incremental GC allocation scheme:
/// * During pause:
///   - New objects are not marked. Can be reclaimed on the next GC run.
/// * During mark phase:
///   - New allocated objects are conservatively marked and cannot be reclaimed in the
///     current GC run. This is necessary because the incremental GC does neither scan
///     nor use write barriers on the call stack.
/// * During evacuation phase:
///   - New allocated objects do not need to be marked since the allocation partition
///     will not be evacuated and reclaimed in the current GC run.
/// Summary: New allocated objects are conservatively retained during an active GC run.
/// `new_object` is the unskewed object pointer.
/// Also import for compiler-generated code to situatively set the mark bit for new heap allocations.
#[no_mangle]
pub unsafe extern "C" fn mark_new_allocation(new_object: *mut Obj) {
    debug_assert!(!is_skewed(new_object as u32));
    let should_mark = match &PHASE {
        Phase::Pause | Phase::Stop | Phase::Evacuate(_) => false,
        Phase::Mark(_) => true,
    };
    if should_mark {
        debug_assert!(!new_object.is_marked());
        new_object.mark();
    }
}

/// Stop the GC before performing upgrade. Otherwise, GC increments
/// on allocation and writes may interfere with the upgrade mechanism
/// that invalidates object tags during stream serialization.
#[no_mangle]
pub unsafe extern "C" fn stop_gc_on_upgrade() {
    PHASE = Phase::Stop;
}
