use motoko_rts_macros::ic_mem_fn;

pub mod mark_stack;
pub mod remembered_set;
#[cfg(debug_assertions)]
pub mod sanity_checks;
pub mod write_barrier;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: crate::memory::Memory>(_mem: &mut M) {
    println!(100, "Schedule incremental GC");
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: crate::memory::Memory>(_mem: &mut M) {
    println!(100, "Incremental GC");
}
