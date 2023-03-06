pub mod array_slicing;
pub mod mark_stack;
pub mod object_table;
pub mod roots;
pub mod write_barrier;
mod young_collection;

use motoko_rts_macros::ic_mem_fn;

use crate::memory::Memory;

use self::young_collection::YoungCollection;

use super::common::{Limits, Roots};

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(mem: &mut M, heap_base: u32) {
    crate::memory::ic::initialize_memory(mem, heap_base, true);
    write_barrier::init_incremental_write_barrier(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    // TODO: Scheduling heuristics:
    // 1. Start combined young and incremental old when old generation has grown by a defined factor.
    // 2. otherwise schedule only young generation has grown more than an amount.
    incremental_gc(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::{get_limits, get_roots, set_limits};

    let new_limits = run_incremental_gc(mem, get_limits(), get_roots());
    set_limits(&new_limits);

    // TODO: If necessary, start or continue the incremental collection
}

pub unsafe fn run_incremental_gc<M: Memory>(mem: &mut M, limits: Limits, roots: Roots) -> Limits {
    const ACTIVE_INCREMENTAL_GC: bool = false;
    // Always collect the young generation before the incremental collection of the old generation.
    let mut gc = YoungCollection::new(mem, limits, roots, ACTIVE_INCREMENTAL_GC);
    let new_limits = gc.run();
    new_limits
}
