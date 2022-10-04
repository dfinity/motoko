pub mod copying;
pub mod experimental;
pub mod mark_compact;
pub mod no;

#[cfg(feature = "ic")]
use crate::types::Bytes;

#[cfg(feature = "ic")]
unsafe fn should_do_gc(max_live: Bytes<u64>) -> bool {
    use crate::memory::ic::{HP, LAST_HP};

    // A factor of last heap size. We allow at most this much allocation before doing GC.
    const HEAP_GROWTH_FACTOR: f64 = 1.5;

    let heap_limit = core::cmp::min(
        (f64::from(LAST_HP) * HEAP_GROWTH_FACTOR) as u64,
        (u64::from(LAST_HP) + max_live.0) / 2,
    );

    u64::from(HP) >= heap_limit
}
