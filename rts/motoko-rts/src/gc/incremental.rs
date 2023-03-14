//! Simple incremental GC.
//!
//! Properties:
//! - Fast reclamation of short-lived objects with generational collection.
//! - Simple incremental compacting collection of the old generation.
//! - Full-heap incremental snapshot-at-the-beginning marking.
//! - Incremental compaction enabled by a central object table.
//!
//! The heap is partitioned in two generations: the old and the young generation.
//!
//! Dynamic heap (starting past the object table):
//!   ┌────────────────┬──────────────────┐
//!   │ Old generation │ Young generation │
//!   └────────────────┴──────────────────┘
//!   ^                ^                  ^
//!   |                |                  |
//!  HEAP_BASE     LAST_HP (last_free)   HP (free)
//!
//! Garbage collection is performed in two steps:
//! 1. Young generation collection (blocking):
//!    The young generation is collected and the surviving objects promoted to the
//!    old generation. This blocks the mutator (non-incremental work) which is acceptable
//!    as the generation tends to be small and the GC work should adapt to the allocation rate.
//! 2. Old generation collection (incremental):
//!    Sporadically, an incremental mark-and-compact collection run is started for the old
//!    generation. The work is performed in multiple increments, where the mutator can resume
//!    work in between. The GC increment is time-bound by a synthetic clock that deterministically
//!    counts work steps.
//!
//! For simplicity, young generation collection always runs before a GC increment of the old generation.
//! Therefore, old generation collection perceives its generation as the full heap and the
//! incremental collector can ignore the generational aspects.
//!
//! Specific aspects of young and old generation collection are explained in `collector.rs`.

pub mod array_slicing;
mod collector;
pub mod mark_stack;
pub mod object_table;
pub mod roots;
pub mod state;
pub mod time;
pub mod write_barrier;

#[cfg(debug_assertions)]
mod sanity_checks;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::{
        common::Strategy,
        incremental::{
            collector::{GarbageCollector, Generation},
            state::{incremental_gc_phase, incremental_gc_state, Phase},
            write_barrier::{create_young_remembered_set, take_young_remembered_set},
        },
    },
    memory::Memory,
    types::Value,
};

use self::{
    state::{is_incremental_gc_running, State},
    time::Time,
};

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(mem: &mut M, heap_base: u32) {
    crate::memory::ic::initialize_memory(mem, heap_base, true);
    write_barrier::init_incremental_write_barrier(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    if decide_incremental_strategy().is_some() {
        incremental_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::{update_statistics, update_strategy};

    let old_heap_size = mem.get_heap_pointer();
    let strategy = decide_incremental_strategy();
    let strategy = strategy.unwrap_or(Strategy::Young); // Use `Strategy::Young` in `--force-gc` mode.

    run_incremental_gc(mem, strategy);

    update_statistics(old_heap_size);
    update_strategy(strategy);
}

#[cfg(feature = "ic")]
unsafe fn decide_incremental_strategy() -> Option<Strategy> {
    use crate::gc::common;

    if is_incremental_gc_running() {
        Some(Strategy::Full)
    } else {
        common::decide_strategy()
    }
}

pub unsafe fn run_incremental_gc<M: Memory>(mem: &mut M, strategy: Strategy) {
    // Always collect the young generation before the incremental collection of the old generation.
    collect_young_generation(mem);
    if strategy == Strategy::Full {
        run_old_generation_increment(mem);
    }
    // New remembered set needs to be allocated in the new young generation.
    create_young_remembered_set(mem);
}

unsafe fn collect_young_generation<M: Memory>(mem: &mut M) {
    let remembered_set = take_young_remembered_set();
    let mark_promoted = is_incremental_gc_running();
    let generation = Generation::young(mem, remembered_set, mark_promoted);
    let mut state = State::new();
    let time = Time::unlimited();
    let mut gc = GarbageCollector::instance(mem, generation, &mut state, time);
    gc.run();
}

const INCREMENT_LIMIT: usize = 3_000_000;

unsafe fn run_old_generation_increment<M: Memory>(mem: &mut M) {
    debug_assert_eq!(mem.get_last_heap_pointer(), mem.get_heap_pointer());
    let generation = Generation::old(mem);
    let state = incremental_gc_state();
    let time = Time::limited(INCREMENT_LIMIT);
    let mut gc = GarbageCollector::instance(mem, generation, state, time);
    gc.run();
}

/// Mark an object by the pre-update write barrier or by promotion from the young generation
/// during an active incremental mark phase.
unsafe fn mark_old_object<M: Memory>(mem: &mut M, object_id: Value) {
    debug_assert!(incremental_gc_phase() == Phase::Mark);
    debug_assert!(object_id.get_object_address() >= mem.get_heap_base());
    debug_assert!(object_id.get_object_address() < mem.get_last_heap_pointer());
    let state = incremental_gc_state();
    let time = Time::limited(0);
    let generation = Generation::old(mem);
    let mut gc = GarbageCollector::instance(mem, generation, state, time);
    gc.mark_object(object_id);
}
