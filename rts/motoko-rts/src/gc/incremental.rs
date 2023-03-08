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
pub mod mark_stack;
pub mod object_table;
mod old_collection;
pub mod roots;
pub mod time;
pub mod write_barrier;
mod young_collection;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::{common::Strategy, incremental::old_collection::OldCollection},
    memory::Memory,
};

use self::{
    old_collection::{incremental_gc_state, is_incremental_gc_running},
    time::BoundedTime,
    young_collection::YoungCollection,
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

    assert_eq!(write_barrier::HEAP_BASE, ic::HEAP_BASE);
    assert_eq!(write_barrier::LAST_HP, ic::LAST_HP);

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

const INCREMENT_LIMIT: usize = 3_000_000;

pub unsafe fn run_incremental_gc<M: Memory>(
    mem: &mut M,
    strategy: Strategy,
    limits: Limits,
    roots: Roots,
) -> Limits {
    // Always collect the young generation before the incremental collection of the old generation.
    let mut limits = limits;
    if strategy == Strategy::Young || strategy == Strategy::Full {
        let mark_promoted_objects = is_incremental_gc_running();
        let mut young_gc = YoungCollection::new(mem, limits, roots, mark_promoted_objects);
        young_gc.run();
        limits = young_gc.get_new_limits();
        if strategy == Strategy::Full {
            let mut state = incremental_gc_state();
            let time = BoundedTime::new(INCREMENT_LIMIT);
            let mut old_gc = OldCollection::instance(mem, limits, state, time);
            old_gc.empty_call_stack_increment(roots);
            limits = old_gc.get_new_limits()
        }
    }
    limits
}
