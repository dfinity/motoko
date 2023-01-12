use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::*};

use self::{
    mark_stack::MarkStack,
    partitioned_heap::{PartitionedHeap, PartitionedHeapIterator, MAX_PARTITIONS},
    phases::{
        evacuation_increment::EvacuationIncrement, mark_increment::MarkIncrement,
        update_increment::UpdateIncrement,
    },
};

pub mod mark_stack;
pub mod partitioned_heap;
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
    use crate::gc::incremental::partitioned_heap::PARTITION_SIZE;
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
    PARTITIONED_HEAP
        .as_ref()
        .unwrap()
        .occupied_size()
        .as_usize()
}

#[cfg(feature = "ic")]
unsafe fn record_increment_stop<M: Memory>() {
    use crate::memory::ic;
    if IncrementalGC::<M>::pausing() {
        let occupation = PARTITIONED_HEAP.as_ref().unwrap().occupied_size();
        ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, occupation);
    }
}

/// GC phases per run. Each of the following phases is performed in potentially multiple increments.
/// 1. Marking: Incremental full-heap snapshot-at-the-beginning marking.
///    Must start on empty call stack.
///     * Concurrent allocations are conservatively marked.
///     * Concurrent pointer writes are handled by the write barrier.
/// 2. Evacuation: Incremental compacting evacuation of high-garbage partitions.
///     * Copying live objects out of the selected partitions to new partitions.
///     * Concurrent accesses to old object locations are handled by pointer forwarding.
/// 3. Updating: Incremental updates of all old pointers to their new forwarded addresses.
///    Must complete on empty call stack.
///     * Also clearing mark bit of all alive objects.
///     * Concurrent copying of old pointer values is intercepted to resolve forwarding.
/// Finally, all the evacuated partitions are freed.

enum Phase {
    Pause,                     // Inactive, waiting for the next GC run.
    Mark(MarkState),           // Incremental marking.
    Evacuate(EvacuationState), // Incremental evacuation compact.
    Update(UpdateState),       // Incremental pointer updates.
    Stop,                      // GC stopped on canister upgrade.
}

struct MarkState {
    heap_base: usize,
    mark_stack: MarkStack,
    complete: bool,
}

struct EvacuationState {
    heap_iterator: PartitionedHeapIterator,
    sweep_address: Option<usize>,
}

struct UpdateState {
    heap_base: usize,
    partition_index: usize,
    scan_address: Option<usize>,
}

/// GC state retained over multiple GC increments.
static mut PHASE: Phase = Phase::Pause;
pub(crate) static mut PARTITIONED_HEAP: Option<PartitionedHeap> = None;

/// Heap limits.
pub struct Limits {
    pub base: usize,
    pub free: usize,
}

/// GC root set.
#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend:
    // - `MarkIncrement::mark_roots()`
    // - `UpdateIncrement.mark_roots()`
}

/// Limit on the number of steps performed in a GC increment.
const INCREMENT_LIMIT: usize = 500_000;

/// Incremental GC.
/// Each GC call has its new GC instance that shares the common GC states `PHASE` and `PARTITIONED_HEAP`.
pub struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
    steps: usize,
}

impl<'a, M: Memory + 'a> IncrementalGC<'a, M> {
    /// (Re-)Initialize the entire incremental garbage collector.
    /// Called on a runtime system start with incremental GC and also during RTS testing.
    pub unsafe fn initialize(heap_base: usize) {
        PHASE = Phase::Pause;
        PARTITIONED_HEAP = Some(PartitionedHeap::new(heap_base));
    }

    /// Each GC schedule point can get a new GC instance that shares the common GC state.
    /// This is because the memory implementation is not stored as global variable.
    pub unsafe fn instance(mem: &'a mut M) -> IncrementalGC<'a, M> {
        IncrementalGC { mem, steps: 0 }
    }

    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * The update phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(&mut self, limits: Limits, roots: Roots) {
        if Self::pausing() {
            self.start_marking(limits.base, roots);
        }
        self.increment();
        if Self::mark_completed() {
            self.start_evacuating();
        }
        if Self::evacuation_completed() {
            self.start_updating(limits.base, roots);
        }
        if Self::updating_completed() {
            self.complete_run();
        }
    }

    /// Pre-update field-level write barrier peforming snapshot-at-the-beginning marking.
    /// The barrier is only effective while the GC is in the mark phase.
    #[inline]
    unsafe fn pre_write_barrier(&mut self, value: Value) {
        if let Phase::Mark(state) = &mut PHASE {
            if value.points_to_or_beyond(state.heap_base) && !state.complete {
                MarkIncrement::instance(self.mem, &mut self.steps).mark_object(value);
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
            Phase::Mark(_) => MarkIncrement::instance(self.mem, &mut self.steps).run(),
            Phase::Evacuate(_) => EvacuationIncrement::instance(self.mem, &mut self.steps).run(),
            Phase::Update(_) => UpdateIncrement::instance(&mut self.steps).run(),
        }
    }

    /// Only to be called when the call stack is empty as pointers on stack are not collected as roots.
    unsafe fn start_marking(&mut self, heap_base: usize, roots: Roots) {
        debug_assert!(Self::pausing());

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(false, false);

        let mark_stack = MarkStack::new(self.mem);
        let state = MarkState {
            heap_base,
            mark_stack,
            complete: false,
        };
        PHASE = Phase::Mark(state);
        let mut increment = MarkIncrement::instance(self.mem, &mut self.steps);
        increment.mark_roots(roots);
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
        debug_assert!(Self::mark_completed());
        let state = EvacuationState {
            heap_iterator: PartitionedHeapIterator::start(),
            sweep_address: None,
        };
        PHASE = Phase::Evacuate(state);
        let mut increment = EvacuationIncrement::instance(self.mem, &mut self.steps);
        increment.initiate_evacuations();
    }

    unsafe fn evacuation_completed() -> bool {
        match &PHASE {
            Phase::Evacuate(state) => state.heap_iterator.current().is_none(),
            _ => false,
        }
    }

    unsafe fn start_updating(&mut self, heap_base: usize, roots: Roots) {
        debug_assert!(Self::evacuation_completed());
        let state = UpdateState {
            heap_base,
            partition_index: 0,
            scan_address: None,
        };
        PHASE = Phase::Update(state);
        let mut increment = UpdateIncrement::instance(&mut self.steps);
        increment.update_roots(roots);
    }

    unsafe fn updating_completed() -> bool {
        match &PHASE {
            Phase::Update(state) => state.partition_index == MAX_PARTITIONS,
            _ => false,
        }
    }

    /// Only to be called when the call stack is empty as pointers on stack are not updated.
    unsafe fn complete_run(&mut self) {
        debug_assert!(Self::updating_completed());
        PARTITIONED_HEAP
            .as_mut()
            .unwrap()
            .free_evacuated_partitions();
        PHASE = Phase::Pause;

        // Note: The memory check only works if the free space is cleared in `PartitionedHeap`.
        // Otherwise, there exist the remainder of garbage objects that have been conceptually freed.
        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_memory(false, false);
    }
}

/// Incremental GC allocation scheme:
/// * During pause:
///   - New objects are not marked. Can be reclaimed on the next GC run.
/// * During mark phase:
///   - New allocated objects are conservatively marked and cannot be reclaimed in the
///     current GC run. This is necessary because the incremental GC does neither scan
///     nor use write barriers on the call stack.
/// * During evacuation and updating phase:
///   - New allocated objects do not need to be marked since the allocation partition
///     will not be evacuated and reclaimed in the current GC run.
/// * When GC is stopped on canister upgrade:
///   - The GC will not resume and thus marking is irrelevant.
/// Summary: New allocated objects are conservatively retained during an active GC run.
/// `new_object` is the unskewed object pointer.
/// Also import for compiler-generated code to situatively set the mark bit for new heap allocations.
#[no_mangle]
pub unsafe extern "C" fn mark_new_allocation(new_object: *mut Obj) {
    debug_assert!(!is_skewed(new_object as u32));
    let should_mark = match &PHASE {
        Phase::Mark(_) => true,
        _ => false,
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
