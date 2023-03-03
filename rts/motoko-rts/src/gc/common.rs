use crate::types::Value;

pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_ptr_loc: *mut Value,
    // For possible future additional roots, please extend the functionality in:
    // * `generational::GenerationalGC::mark_root_set`
    // * `generational::GenerationalGC::thread_initial_phase`
    // * `incremental::YoungCollection::mark_root_set`
}

#[derive(Clone)]
pub struct Limits {
    pub base: usize,
    pub last_free: usize, // This separates the old generation from the young generation.
    pub free: usize,
}

#[cfg(feature = "ic")]
pub unsafe fn get_limits() -> Limits {
    use crate::memory::ic;
    assert!(ic::HP >= ic::LAST_HP);
    assert!(ic::LAST_HP >= ic::HEAP_BASE);
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
        continuation_table_ptr_loc: crate::continuation_table::continuation_table_loc(),
    }
}

#[cfg(feature = "ic")]
pub unsafe fn set_limits(limits: &Limits) {
    use crate::memory::ic;
    ic::HP = limits.free as u32;
    ic::LAST_HP = limits.free as u32;
    assert!(ic::HP >= ic::LAST_HP);
    assert!(ic::LAST_HP >= ic::HEAP_BASE);
}
