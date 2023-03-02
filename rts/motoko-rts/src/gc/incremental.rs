pub mod object_table;
pub mod write_barrier;

use motoko_rts_macros::ic_mem_fn;

#[cfg(feature = "ic")]
use crate::memory::Memory;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(mem: &mut M, heap_base: u32) {
    crate::memory::ic::initialize_memory(mem, heap_base, true);
    write_barrier::init_incremental_write_barrier(mem);

    println!(100, "INITIALIZE INCREMENTAL GC");
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(_mem: &mut M) {
    println!(100, "SCHEDULE INCREMENTAL GC");
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(_mem: &mut M) {
    println!(100, "INCREMENTAL GC");
}
