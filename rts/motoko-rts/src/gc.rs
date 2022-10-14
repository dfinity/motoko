pub mod copying;
pub mod generational;
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

static mut SHOW_GC_MESSAGES: bool = false;
static mut GC_RUN: usize = 0;

#[no_mangle]
pub unsafe fn show_gc_messages() {
    SHOW_GC_MESSAGES = true;
}

pub unsafe fn show_gc_start(name: &str) {
    GC_RUN += 1;
    if SHOW_GC_MESSAGES {
        println!(100, "INFO: {name} run {GC_RUN} starts ...");
    }
}

pub unsafe fn show_gc_stop(name: &str) {
    if SHOW_GC_MESSAGES {
        println!(100, "INFO: {name} run {GC_RUN} stops ...");
    }
}
