use crate::memory::Memory;
use crate::region::{NO_REGION, REGION_0};
use crate::types::Value;

use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn]
pub unsafe fn region0_get<M: Memory>(_mem: &mut M) -> Value {
    assert_ne!(REGION_0, NO_REGION);
    REGION_0
}

// Expose Region0 object to GC algorithms.
#[allow(dead_code)]
#[cfg(feature = "ic")]
pub(crate) unsafe fn region0_get_ptr_loc() -> *mut Value {
    &mut REGION_0
}
