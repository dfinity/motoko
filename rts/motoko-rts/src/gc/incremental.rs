use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::*};

use self::{
    mark_stack::MarkStack,
    partition_map::{PartitionMap, MAX_PARTITIONS},
    phases::{evacuation_phase::EvacuationIncrement, marking_phase::MarkingIncrement},
};

pub mod mark_stack;
pub mod partition_map;
mod phases;
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
    Mark(MarkingState),
    Evacuate(EvacuationState),
    Stop, // On canister upgrade.
}

struct MarkingState {
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
/// Each GC call has its new GC instance that shares the common GC states `PHASE` and `PARTITION_MAP`.
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
            // TODO
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
                MarkingIncrement::instance(self.mem).mark_object(value);
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
            Phase::Mark(_) => MarkingIncrement::instance(self.mem).run(),
            Phase::Evacuate(_) => EvacuationIncrement::instance(self.mem).run(),
        }
    }

    unsafe fn start_marking(&mut self, heap_base: usize, roots: Roots) {
        debug_assert!(Self::pausing());

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(false);

        let mark_stack = MarkStack::new(self.mem);
        let state = MarkingState {
            heap_base,
            mark_stack,
            complete: false,
        };
        PHASE = Phase::Mark(state);
        let mut increment = MarkingIncrement::instance(self.mem);
        increment.mark_roots(roots);
        increment.run();
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
        let mut increment = EvacuationIncrement::instance(self.mem);
        increment.initiate_evacuations();
        increment.run();
    }

    unsafe fn evacuation_completed() -> bool {
        match &PHASE {
            Phase::Evacuate(state) => state.partition_index == MAX_PARTITIONS,
            _ => false,
        }
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
