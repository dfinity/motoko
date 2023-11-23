//! Incremental evacuation-compacting GC.
//!
//! Properties:
//! - All GC pauses have bounded short time.
//! - Full-heap snapshot-at-the-beginning marking.
//! - Focus on reclaiming high-garbage partitions.
//! - Compacting heap space with partition evacuations.
//! - Incremental copying enabled by forwarding pointers.
use core::cell::RefCell;

use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::*, visitor::visit_pointer_fields};

use self::{
    partitioned_heap::{PartitionedHeap, PartitionedHeapIterator, UNINITIALIZED_HEAP},
    phases::{
        evacuation_increment::EvacuationIncrement,
        mark_increment::{MarkIncrement, MarkState},
        update_increment::UpdateIncrement,
    },
    roots::Roots,
    time::BoundedTime,
};

pub mod array_slicing;
pub mod barriers;
pub mod mark_bitmap;
pub mod mark_stack;
pub mod partitioned_heap;
mod phases;
pub mod roots;
#[cfg(feature = "memory_check")]
pub mod sanity_checks;
pub mod sort;
pub mod time;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    IncrementalGC::<M>::initialize(mem, ic::get_aligned_heap_base());
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    let state = STATE.borrow();
    assert!(state.phase != Phase::Stop);
    let running = state.phase != Phase::Pause;
    if running || should_start() {
        incremental_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use self::roots::root_set;
    let state = STATE.get_mut();
    if state.phase == Phase::Pause {
        record_gc_start::<M>();
    }
    IncrementalGC::instance(mem, state).empty_call_stack_increment(root_set());
    if state.phase == Phase::Pause {
        record_gc_stop::<M>();
    }
}

#[cfg(feature = "ic")]
static mut LAST_ALLOCATIONS: Bytes<u64> = Bytes(0u64);

#[cfg(feature = "ic")]
unsafe fn should_start() -> bool {
    use self::partitioned_heap::PARTITION_SIZE;
    use crate::constants::GB;
    use crate::memory::ic::partitioned_memory;

    const CRITICAL_HEAP_LIMIT: Bytes<u32> = Bytes(2 * GB as u32);
    const CRITICAL_GROWTH_THRESHOLD: f64 = 0.01;
    const MEDIUM_HEAP_LIMIT: Bytes<u32> = Bytes(GB as u32);
    const MEDIUM_GROWTH_THRESHOLD: f64 = 0.35;
    const LOW_GROWTH_THRESHOLD: f64 = 0.65;

    let heap_size = partitioned_memory::get_heap_size();
    let growth_threshold = if heap_size > CRITICAL_HEAP_LIMIT {
        CRITICAL_GROWTH_THRESHOLD
    } else if heap_size > MEDIUM_HEAP_LIMIT {
        MEDIUM_GROWTH_THRESHOLD
    } else {
        LOW_GROWTH_THRESHOLD
    };

    let current_allocations = partitioned_memory::get_total_allocations();
    debug_assert!(current_allocations >= LAST_ALLOCATIONS);
    let absolute_growth = current_allocations - LAST_ALLOCATIONS;
    let relative_growth = absolute_growth.0 as f64 / heap_size.as_usize() as f64;
    relative_growth > growth_threshold && heap_size.as_usize() >= PARTITION_SIZE
}

#[cfg(feature = "ic")]
unsafe fn record_gc_start<M: Memory>() {
    use crate::memory::ic::partitioned_memory;
    LAST_ALLOCATIONS = partitioned_memory::get_total_allocations();
}

#[cfg(feature = "ic")]
unsafe fn record_gc_stop<M: Memory>() {
    use crate::memory::ic::{self, partitioned_memory};

    let heap_size = partitioned_memory::get_heap_size();
    let static_size = Bytes(ic::get_aligned_heap_base() as u32);
    debug_assert!(heap_size >= static_size);
    let dynamic_size = heap_size - static_size;
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, dynamic_size);
}

/// GC phases per run. Each of the following phases is performed in potentially multiple increments.
/// 1. Marking: Incremental full-heap snapshot-at-the-beginning marking.
///    Must start on empty call stack.
///     * Concurrent allocations are conservatively marked.
///     * Concurrent pointer writes are handled by the write barrier.
/// 2. Evacuation: Incremental evacuation-compaction of high-garbage partitions.
///     * Copying live objects out of the selected partitions to new partitions.
///     * Concurrent accesses to old object locations are handled by pointer forwarding.
/// 3. Updating: Incremental updates of all old pointers to their new forwarded addresses.
///    Must complete on empty call stack.
///     * Concurrent copying of old pointer values is intercepted to resolve forwarding.
/// Finally, all the evacuated and temporary partitions are freed.
/// The temporary partitions store mark bitmaps.

/// The limit on the GC increment has a fix base with a linear increase depending on the number of
/// allocations that were performed during a running GC. The allocation-proportional term adapts
/// to the allocation rate and helps the GC to reduce reclamation latency.
const INCREMENT_BASE_LIMIT: usize = 3_500_000; // Increment limit without concurrent allocations.
const INCREMENT_ALLOCATION_FACTOR: usize = 10; // Additional time factor per concurrent allocation.

// Performance note: Storing the phase-specific state in the enum would be nicer but it is much slower.
#[derive(PartialEq)]
enum Phase {
    Pause,    // Inactive, waiting for the next GC run.
    Mark,     // Incremental marking.
    Evacuate, // Incremental evacuation compaction.
    Update,   // Incremental pointer updates.
    Stop,     // GC stopped on canister upgrade.
}

pub struct State {
    phase: Phase,
    partitioned_heap: PartitionedHeap,
    allocation_count: usize, // Number of allocations during an active GC run.
    mark_state: Option<MarkState>,
    iterator_state: Option<PartitionedHeapIterator>,
    running_increment: bool, // GC increment is active.
}

/// GC state retained over multiple GC increments.
static mut STATE: RefCell<State> = RefCell::new(State {
    phase: Phase::Pause,
    partitioned_heap: UNINITIALIZED_HEAP,
    allocation_count: 0,
    mark_state: None,
    iterator_state: None,
    running_increment: false,
});

/// Incremental GC.
/// Each GC call has its new GC instance that shares the common GC state `STATE`.
pub struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
    state: &'a mut State,
    time: BoundedTime,
}

impl<'a, M: Memory + 'a> IncrementalGC<'a, M> {
    /// (Re-)Initialize the entire incremental garbage collector.
    /// Called on a runtime system start with incremental GC and also during RTS testing.
    pub unsafe fn initialize(mem: &'a mut M, heap_base: usize) {
        let state = STATE.get_mut();
        state.phase = Phase::Pause;
        state.partitioned_heap = PartitionedHeap::new(mem, heap_base);
        state.allocation_count = 0;
        state.mark_state = None;
        state.iterator_state = None;
        state.running_increment = false;
    }

    /// Each GC schedule point can get a new GC instance that shares the common GC state.
    /// This is because the memory implementation is not stored as global variable.
    pub unsafe fn instance(mem: &'a mut M, state: &'a mut State) -> IncrementalGC<'a, M> {
        debug_assert!(state.partitioned_heap.is_initialized());
        let limit = usize::saturating_add(
            INCREMENT_BASE_LIMIT,
            usize::saturating_mul(state.allocation_count, INCREMENT_ALLOCATION_FACTOR),
        );
        state.allocation_count = 0;
        let time = BoundedTime::new(limit);
        IncrementalGC { mem, state, time }
    }

    /// Regular GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase can only be started on an empty call stack.
    /// * The update phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(&mut self, roots: Roots) {
        debug_assert!(!self.state.running_increment);
        self.state.running_increment = true;
        assert!(self.state.phase != Phase::Stop);
        if self.pausing() {
            self.start_marking(roots);
        }
        if self.state.phase == Phase::Mark {
            MarkIncrement::instance(self.mem, self.state, &mut self.time).run();
        }
        if self.mark_completed() {
            self.start_evacuating(roots);
        }
        if self.state.phase == Phase::Evacuate {
            EvacuationIncrement::instance(self.mem, self.state, &mut self.time).run();
        }
        if self.evacuation_completed() {
            self.start_updating(roots);
        }
        if self.state.phase == Phase::Update {
            UpdateIncrement::instance(self.state, &mut self.time).run();
        }
        if self.updating_completed() {
            self.complete_run(roots);
        }
        self.state.running_increment = false;
    }

    unsafe fn pausing(&mut self) -> bool {
        self.state.phase == Phase::Pause
    }

    /// Only to be called when the call stack is empty as pointers on stack are not collected as roots.
    unsafe fn start_marking(&mut self, roots: Roots) {
        debug_assert!(self.pausing());

        self.state.phase = Phase::Mark;
        MarkIncrement::start_phase(self.mem, self.state, &mut self.time);
        let mut increment = MarkIncrement::instance(self.mem, self.state, &mut self.time);
        increment.mark_roots(roots);
    }

    unsafe fn mark_completed(&self) -> bool {
        self.state.phase == Phase::Mark && MarkIncrement::<M>::mark_completed(self.state)
    }

    unsafe fn check_mark_completion(&mut self, _roots: Roots) {
        #[cfg(feature = "memory_check")]
        {
            sanity_checks::check_memory(
                self.mem,
                &mut self.state.partitioned_heap,
                _roots,
                sanity_checks::CheckerMode::MarkCompletion,
            );
        }
    }

    unsafe fn start_evacuating(&mut self, roots: Roots) {
        self.check_mark_completion(roots);
        debug_assert!(self.mark_completed());
        MarkIncrement::<M>::complete_phase(self.state);
        self.state.phase = Phase::Evacuate;
        EvacuationIncrement::<M>::start_phase(self.state);
    }

    unsafe fn evacuation_completed(&self) -> bool {
        self.state.phase == Phase::Evacuate
            && EvacuationIncrement::<M>::evacuation_completed(self.state)
    }

    unsafe fn start_updating(&mut self, roots: Roots) {
        debug_assert!(self.evacuation_completed());
        EvacuationIncrement::<M>::complete_phase(self.state);
        self.state.phase = Phase::Update;
        UpdateIncrement::start_phase(self.state);
        let mut increment = UpdateIncrement::instance(self.state, &mut self.time);
        increment.update_roots(roots);
    }

    unsafe fn updating_completed(&self) -> bool {
        self.state.phase == Phase::Update && UpdateIncrement::update_completed(self.state)
    }

    /// Only to be called when the call stack is empty as pointers on stack are not updated.
    unsafe fn complete_run(&mut self, roots: Roots) {
        debug_assert!(self.updating_completed());
        UpdateIncrement::complete_phase(self.state);
        self.state.phase = Phase::Pause;
        self.check_update_completion(roots);
    }

    unsafe fn check_update_completion(&mut self, _roots: Roots) {
        #[cfg(feature = "memory_check")]
        {
            sanity_checks::check_memory(
                self.mem,
                &mut self.state.partitioned_heap,
                _roots,
                sanity_checks::CheckerMode::UpdateCompletion,
            );
        }
    }
}

/// Write barrier to be called BEFORE a potential overwrite of a pointer value.
/// `overwritten_value` (skewed if a pointer) denotes the value that will be overwritten.
/// The barrier can be conservatively called even if the overwritten value is not a pointer.
/// The barrier is only effective while the GC is in the mark phase.
unsafe fn pre_write_barrier<M: Memory>(mem: &mut M, state: &mut State, overwritten_value: Value) {
    if state.phase == Phase::Mark {
        let base_address = state.partitioned_heap.base_address();
        if overwritten_value.points_to_or_beyond(base_address) {
            let mut time = BoundedTime::new(0);
            let mut increment = MarkIncrement::instance(mem, state, &mut time);
            increment.mark_object(overwritten_value);
        }
    }
}

/// Allocation barrier to be called AFTER a new object allocation.
/// `new_object` is the skewed pointer of the newly allocated and initialized object.
/// The new object needs to be fully initialized, except for the payload of a blob.
/// The barrier is only effective during a running GC.
unsafe fn post_allocation_barrier(state: &mut State, new_object: Value) {
    if state.phase == Phase::Mark || state.phase == Phase::Evacuate {
        mark_new_allocation(state, new_object);
    } else if state.phase == Phase::Update {
        update_new_allocation(state, new_object);
    }
}

/// Mark a new object during the mark phase and evacuation phase.
/// `new_object` is the skewed pointer of a newly allocated object.
///
/// Incremental GC allocation scheme:
/// * During the pause:
///   - No marking. New objects can be reclaimed in the next GC round if they become garbage by then.
/// * During the mark phase:
///   - New allocated objects are conservatively marked and cannot be reclaimed in the
///     current GC run. This is necessary because the incremental GC does neither scan
///     nor use write barriers on the call stack. The fields in the newly allocated array
///     do not need to be visited during the mark phase due to the snapshot-at-the-beginning
///     consistency.
/// * During the evacuation phase:
///   - Mark new objects such that their fields are updated in the subsequent update phase.
///     The fields may still point to old object locations that are forwarded.
/// * During the update phase
///   - New objects do not need to be marked as they are allocated in non-evacuated partitions.
/// * When GC is stopped on canister upgrade:
///   - The GC will not resume and thus marking is irrelevant.
///   - This is necessary because the upgrade serialization alters tags to store other information.
unsafe fn mark_new_allocation(state: &mut State, new_object: Value) {
    debug_assert!(state.phase == Phase::Mark || state.phase == Phase::Evacuate);
    let object = new_object.get_ptr() as *mut Obj;
    let unmarked_before = state.partitioned_heap.mark_object(object);
    debug_assert!(unmarked_before);
}

/// Update the pointer fields during the update phase.
/// This is to ensure that new allocation do not contain any old pointers referring to
/// forwarded objects.
/// The object must be fully initialized, except for the payload of a blob.
/// `new_object` is the skewed pointer of a newly allocated and initialized object.
///
/// Incremental GC update scheme:
/// * During the mark phase and a pause:
///   - No pointers to forwarded pointers exist in alive objects.
/// * During the evacuation phase:
///   - The fields may point to old locations that are forwarded.
/// * During the update phase:
///   - All old pointers to forwarded objects must be updated to refer to the corresponding
///     new object locations. Since the mutator may copy old pointers around, all allocations
///     and pointer writes must be handled by barriers.
///   - Allocation barrier: Resolve the forwarding for all pointers in the new allocation.
///   - Write barrier: Resolve forwarding for the written pointer value.
/// * When the GC is stopped on canister upgrade:
///   - The GC will not resume and thus pointer updates are irrelevant. The runtime system
///     continues to resolve the forwarding for all remaining old pointers.
unsafe fn update_new_allocation(state: &State, new_object: Value) {
    debug_assert!(state.phase == Phase::Update);
    if state.partitioned_heap.updates_needed() {
        let object = new_object.get_ptr() as *mut Obj;
        visit_pointer_fields(
            &mut (),
            object,
            object.tag(),
            state.partitioned_heap.base_address(),
            |_, field| {
                *field = (*field).forward_if_possible();
            },
            |_, _, array| array.len(),
        );
    }
}

/// Count a concurrent allocation to increase the next scheduled GC increment.
unsafe fn count_allocation(state: &mut State) {
    if state.phase != Phase::Pause {
        state.allocation_count += 1;
    }
}

/// Stop the GC before performing upgrade. This is only a safe-guard since
/// the compiler must not schedule the GC during stabilization anyway.
#[no_mangle]
pub unsafe extern "C" fn stop_gc_on_upgrade() {
    STATE.get_mut().phase = Phase::Stop;
}

pub unsafe fn incremental_gc_state() -> &'static mut State {
    STATE.get_mut()
}

pub unsafe fn get_partitioned_heap() -> &'static mut PartitionedHeap {
    debug_assert!(STATE.get_mut().partitioned_heap.is_initialized());
    &mut STATE.get_mut().partitioned_heap
}

#[cfg(feature = "ic")]
use crate::constants::MB;

/// Additional memory reserve in bytes for the GC.
/// * To allow mark bitmap allocation, i.e. max. 128 MB in 4 GB address space.
/// * 512 MB of free space for evacuations/compactions.
#[cfg(feature = "ic")]
const GC_MEMORY_RESERVE: usize = (128 + 512) * MB;

#[cfg(feature = "ic")]
pub unsafe fn memory_reserve() -> usize {
    use crate::memory::GENERAL_MEMORY_RESERVE;

    let additional_reserve = if STATE.borrow().running_increment {
        0
    } else {
        GC_MEMORY_RESERVE
    };
    GENERAL_MEMORY_RESERVE + additional_reserve
}
