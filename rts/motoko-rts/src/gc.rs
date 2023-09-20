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
unsafe fn should_do_gc(max_live: crate::types::Bytes<usize>) -> bool {
    use crate::memory::ic::linear_memory::{get_hp_unskewed, LAST_HP};

    // A factor of last heap size. We allow at most this much allocation before doing GC.
    const HEAP_GROWTH_FACTOR: f64 = 1.5;

    let heap_limit = core::cmp::min(
        (LAST_HP as f64 * HEAP_GROWTH_FACTOR) as usize,
        (LAST_HP + max_live.as_usize()) / 2,
    );

    get_hp_unskewed() >= heap_limit
}
