use crate::types::Value;

#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_location: *mut Value,
    // For possible future additional roots, please extend the functionality in:
    // * `generational::GenerationalGC::mark_root_set`
    // * `generational::GenerationalGC::thread_initial_phase`
    // * `incremental::roots::visit_roots`
}

#[cfg(feature = "ic")]
pub unsafe fn get_roots() -> Roots {
    use crate::memory::ic;
    Roots {
        static_roots: ic::get_static_roots(),
        continuation_table_location: crate::continuation_table::continuation_table_loc(),
    }
}

#[cfg(feature = "ic")]
pub unsafe fn update_statistics(old_heap_size: usize) {
    use crate::{memory::ic, types::Bytes};

    let new_heap_size = ic::HP as usize;
    let live_size = Bytes(new_heap_size as u32);
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size);
    debug_assert!(old_heap_size >= new_heap_size);
    let reclaimed = old_heap_size - new_heap_size;
    ic::RECLAIMED += Bytes(reclaimed as u64);
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Strategy {
    Young,
    Full,
}

#[cfg(feature = "ic")]
const MB: usize = 1024 * 1024;

#[cfg(feature = "ic")]
static mut OLD_GENERATION_THRESHOLD: usize = 32 * MB;

#[cfg(feature = "ic")]
static mut PASSED_CRITICAL_LIMIT: bool = false;

#[cfg(feature = "ic")]
const CRITICAL_MEMORY_LIMIT: usize = (4096 - 512) * MB;

#[cfg(feature = "ic")]
pub unsafe fn decide_strategy() -> Option<Strategy> {
    use crate::memory::ic;

    const YOUNG_GENERATION_THRESHOLD: usize = 8 * MB;
    if (ic::HP as usize) >= CRITICAL_MEMORY_LIMIT && !PASSED_CRITICAL_LIMIT {
        PASSED_CRITICAL_LIMIT = true;
        Some(Strategy::Full)
    } else if ic::LAST_HP - ic::HEAP_BASE > OLD_GENERATION_THRESHOLD as u32 {
        Some(Strategy::Full)
    } else if ic::HP - ic::LAST_HP > YOUNG_GENERATION_THRESHOLD as u32 {
        Some(Strategy::Young)
    } else {
        None
    }
}

#[cfg(feature = "ic")]
pub unsafe fn update_strategy(strategy: Strategy) {
    use crate::memory::ic;

    const GROWTH_RATE: f64 = 2.0;
    if strategy == Strategy::Full {
        debug_assert_eq!(ic::HP, ic::LAST_HP);
        let heap_size = ic::HP - ic::HEAP_BASE;
        OLD_GENERATION_THRESHOLD = (heap_size as f64 * GROWTH_RATE) as usize;
        if (ic::HP as usize) < CRITICAL_MEMORY_LIMIT {
            PASSED_CRITICAL_LIMIT = false
        }
    }
}
