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
//! Specific aspects of young and old generation collection are explained in `young_collection.rs`
//! and `old_collection.rs`, respectively.

pub mod array_slicing;
mod collector;
pub mod mark_stack;
pub mod object_table;
pub mod roots;
mod state;
pub mod time;
pub mod write_barrier;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::{
        common::Strategy,
        incremental::{
            collector::{GarbageCollector, Generation},
            state::incremental_gc_state,
            write_barrier::{create_young_remembered_set, take_young_remembered_set},
        },
    },
    memory::Memory,
};

use self::{
    state::{is_incremental_gc_running, State},
    time::Time,
};

use super::common::{Limits, Roots};

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(mem: &mut M, heap_base: u32) {
    crate::memory::ic::initialize_memory(mem, heap_base, true);
    write_barrier::init_incremental_write_barrier(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::get_limits;

    let limits = get_limits();
    if decide_incremental_strategy(limits).is_some() {
        incremental_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::{
        get_limits, get_roots, set_limits, update_statistics, update_strategy,
    };
    use crate::memory::ic;

    debug_assert_eq!(write_barrier::HEAP_BASE, ic::HEAP_BASE);
    debug_assert_eq!(write_barrier::LAST_HP, ic::LAST_HP);

    let old_limits = get_limits();
    let strategy = decide_incremental_strategy(old_limits);
    let strategy = strategy.unwrap_or(Strategy::Young); // Use `Strategy::Young` in `--force-gc` mode.

    let new_limits = run_incremental_gc(mem, strategy, old_limits, get_roots());

    set_limits(new_limits);
    update_statistics(old_limits, new_limits);
    update_strategy(strategy, new_limits);
}

#[cfg(feature = "ic")]
unsafe fn decide_incremental_strategy(limits: Limits) -> Option<Strategy> {
    use crate::gc::common;

    if is_incremental_gc_running() {
        Some(Strategy::Full)
    } else {
        common::decide_strategy(limits)
    }
}

pub unsafe fn run_incremental_gc<M: Memory>(
    mem: &mut M,
    strategy: Strategy,
    limits: Limits,
    roots: Roots,
) -> Limits {
    // Always collect the young generation before the incremental collection of the old generation.
    let mut limits = limits;
    if strategy == Strategy::Young || strategy == Strategy::Full {
        collect_young_generation(mem, &mut limits, roots);
        if strategy == Strategy::Full {
            run_old_generation_increment(mem, &mut limits, roots);
        }
        // New remembered set needs to be allocated in the new young generation.
        create_young_remembered_set(mem, limits.last_free);
    }
    limits
}

unsafe fn collect_young_generation<M: Memory>(mem: &mut M, limits: &mut Limits, roots: Roots) {
    let remembered_set = take_young_remembered_set();
    let mark_promoted = is_incremental_gc_running();
    let generation = Generation::young(*limits, remembered_set, mark_promoted);
    let mut state = State::new();
    let time = Time::unlimited();
    let mut gc = GarbageCollector::instance(mem, generation, &mut state, time);
    gc.run(roots);
    limits.set_heap_end(gc.get_heap_end());
}

const INCREMENT_LIMIT: usize = 3_000_000;

unsafe fn run_old_generation_increment<M: Memory>(mem: &mut M, limits: &mut Limits, roots: Roots) {
    debug_assert_eq!(limits.young_generation_size(), 0);
    let generation = Generation::old(*limits);
    let state = incremental_gc_state();
    let time = Time::limited(INCREMENT_LIMIT);
    let mut gc = GarbageCollector::instance(mem, generation, state, time);
    gc.run(roots);
    limits.set_heap_end(gc.get_heap_end());
}
