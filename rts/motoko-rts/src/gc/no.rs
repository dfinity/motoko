//! Disabled GC, for benchmark measurements.

use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_no_gc<M: crate::memory::Memory>(mem: &mut M) {
    no_gc(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn no_gc<M: crate::memory::Memory>(_mem: &mut M) {
    println!(100, "INFO: GC DISABLED ...");
}
