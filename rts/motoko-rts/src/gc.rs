pub mod copying;
pub mod mark_compact;

#[cfg(feature = "ic")]
use crate::types::Bytes;

#[cfg(feature = "ic")]
static mut LAST_TOTAL_ALLOC: u32 = 0;

#[cfg(feature = "ic")]
unsafe fn should_do_gc<P: crate::page_alloc::PageAlloc>(
    heap: &crate::space::Space<P>,
    max_live: Bytes<u64>,
) -> bool {
    // A factor of last heap size. We allow at most this much allocation before doing GC.
    const HEAP_GROWTH_FACTOR: f64 = 1.5;

    let heap_limit = core::cmp::min(
        (f64::from(LAST_TOTAL_ALLOC) * HEAP_GROWTH_FACTOR) as u64,
        (u64::from(LAST_TOTAL_ALLOC) + max_live.0) / 2,
    );

    heap.total_alloc() as u64 >= heap_limit
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    // TODO
    Bytes(0)
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    // TODO
    Bytes(0)
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    // TODO
    Bytes(0)
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    // TODO
    Bytes(0)
}
