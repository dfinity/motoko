//! Disabled GC, for benchmark measurements.

use motoko_rts_macros::ic_mem_fn;
use crate::memory::Memory;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_no_gc<M: Memory>(mem: &mut M) {
    no_gc(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn no_gc<M: Memory>(mem: &mut M) {
    println!(100, "INFO: GC DISABLED ...");
}
