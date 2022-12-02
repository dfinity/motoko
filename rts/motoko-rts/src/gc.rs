pub mod copying;
pub mod generational;
pub mod incremental;
pub mod mark_compact;

#[cfg(feature = "ic")]
use crate::memory::Memory;
#[cfg(feature = "ic")]
use crate::types::Bytes;

#[cfg(feature = "ic")]
unsafe fn should_do_gc<M: Memory>(mem: &mut M, max_live: Bytes<u64>) -> bool {
    // A factor of last heap size. We allow at most this much allocation before doing GC.
    const HEAP_GROWTH_FACTOR: f64 = 1.5;

    let heap_limit = core::cmp::min(
        (f64::from(mem.last_heap_pointer()) * HEAP_GROWTH_FACTOR) as u64,
        (u64::from(mem.last_heap_pointer()) + max_live.0) / 2,
    );

    u64::from(mem.heap_pointer()) >= heap_limit
}
