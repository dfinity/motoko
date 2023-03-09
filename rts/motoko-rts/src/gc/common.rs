use crate::{constants::WORD_SIZE, types::Value};

#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_location: *mut Value,
    // For possible future additional roots, please extend the functionality in:
    // * `generational::GenerationalGC::mark_root_set`
    // * `generational::GenerationalGC::thread_initial_phase`
    // * `incremental::roots::visit_roots`
}

#[derive(Clone, Copy)]
pub struct Limits {
    pub base: usize,
    pub last_free: usize, // This separates the old generation from the young generation.
    pub free: usize,
}

impl Limits {
    pub fn set_heap_end(&mut self, free: usize) {
        assert_eq!(free % WORD_SIZE as usize, 0);
        assert!(self.base <= free);
        self.free = free;
        self.last_free = free;
    }
}

impl Limits {
    pub fn heap_size(&self) -> usize {
        assert!(self.base <= self.free);
        self.free - self.base
    }

    pub fn old_generation_size(&self) -> usize {
        assert!(self.base <= self.last_free);
        self.last_free - self.base
    }

    pub fn young_generation_size(&self) -> usize {
        assert!(self.last_free <= self.free);
        self.free - self.last_free
    }
}

#[cfg(feature = "ic")]
pub unsafe fn get_limits() -> Limits {
    use crate::memory::ic;
    assert!(ic::LAST_HP <= ic::HP);
    assert!(ic::HEAP_BASE <= ic::LAST_HP);
    Limits {
        base: ic::HEAP_BASE as usize,
        last_free: ic::LAST_HP as usize,
        free: ic::HP as usize,
    }
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
pub unsafe fn set_limits(limits: Limits) {
    use crate::memory::ic;
    assert!(limits.base == ic::HEAP_BASE as usize);
    assert!(limits.base <= limits.free);
    assert!(limits.last_free == limits.free);
    ic::HP = limits.free as u32;
    ic::LAST_HP = ic::HP;
}

#[cfg(feature = "ic")]
pub unsafe fn update_statistics(old_limits: Limits, new_limits: Limits) {
    use crate::{memory::ic, types::Bytes};

    let live_size = Bytes(new_limits.heap_size() as u32);
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size);
    assert!(old_limits.heap_size() >= new_limits.heap_size());
    let reclaimed = old_limits.heap_size() - new_limits.heap_size();
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
pub unsafe fn decide_strategy(limits: Limits) -> Option<Strategy> {
    const YOUNG_GENERATION_THRESHOLD: usize = 8 * MB;

    if limits.free >= CRITICAL_MEMORY_LIMIT && !PASSED_CRITICAL_LIMIT {
        PASSED_CRITICAL_LIMIT = true;
        Some(Strategy::Full)
    } else if limits.old_generation_size() > OLD_GENERATION_THRESHOLD {
        Some(Strategy::Full)
    } else if limits.young_generation_size() > YOUNG_GENERATION_THRESHOLD {
        Some(Strategy::Young)
    } else {
        None
    }
}

#[cfg(feature = "ic")]
pub unsafe fn update_strategy(strategy: Strategy, limits: Limits) {
    const GROWTH_RATE: f64 = 2.0;

    if strategy == Strategy::Full {
        assert!(limits.heap_size() == limits.old_generation_size());
        OLD_GENERATION_THRESHOLD = (limits.heap_size() as f64 * GROWTH_RATE) as usize;
        if limits.free < CRITICAL_MEMORY_LIMIT {
            PASSED_CRITICAL_LIMIT = false
        }
    }
}
