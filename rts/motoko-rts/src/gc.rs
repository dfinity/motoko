#[non_incremental_gc]
pub mod copying;
#[non_incremental_gc]
pub mod generational;
#[incremental_gc]
pub mod incremental;
#[non_incremental_gc]
pub mod mark_compact;

pub mod remembered_set;

use motoko_rts_macros::*;

#[cfg(feature = "ic")]
#[non_incremental_gc]
unsafe fn should_do_gc(max_live: crate::types::Bytes<u64>) -> bool {
    use crate::memory::ic::linear_memory::{get_hp_unskewed, LAST_HP};

    // A factor of last heap size. We allow at most this much allocation before doing GC.
    const HEAP_GROWTH_FACTOR: f64 = 1.5;

    let heap_limit = core::cmp::min(
        (f64::from(LAST_HP as u32) * HEAP_GROWTH_FACTOR) as u64,
        (u64::from(LAST_HP as u32) + max_live.0) / 2,
    );

    u64::from(get_hp_unskewed() as u32) >= heap_limit
}

#[non_incremental_gc]
static mut ENABLED: bool = true;

/// Stop all GCs during stabilzation and destabilization.
/// * During stabilzation, the heap is invalidated because of the forwarding objects of Cheney's algorithm.
/// * During destabilization, the scan stack is in use and its implementation does not support stack block movements.
#[non_incremental_gc]
pub unsafe fn stop_gc_before_upgrade() {
    ENABLED = false;
}

#[no_mangle]
#[non_incremental_gc]
pub unsafe extern "C" fn start_gc_after_upgrade() {
    ENABLED = true;
}

#[non_incremental_gc]
#[cfg(feature = "ic")]
unsafe fn is_gc_enabled() -> bool {
    ENABLED
}

#[incremental_gc]
pub unsafe fn stop_gc_before_upgrade() {
    self::incremental::stop_gc_before_upgrade();
}

#[no_mangle]
#[incremental_gc]
pub unsafe extern "C" fn start_gc_after_upgrade() {
    self::incremental::start_gc_after_upgrade();
}
