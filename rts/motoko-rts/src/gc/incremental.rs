use motoko_rts_macros::ic_mem_fn;

#[cfg(feature = "ic")]
use crate::memory::Memory;

pub mod barriers;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_incremental_gc<M: Memory>(_mem: &mut M) {
    use crate::memory::ic;
    ic::initialize_memory();
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(_mem: &mut M) {}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(_mem: &mut M) {}
