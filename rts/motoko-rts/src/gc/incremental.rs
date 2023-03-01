pub mod object_table;

use motoko_rts_macros::ic_mem_fn;

#[cfg(feature = "ic")]
use crate::memory::Memory;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(_mem: &mut M) {
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
