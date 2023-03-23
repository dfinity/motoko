//! Barriers for the incremental GC used by the compiler:
//! * Write barrier
//! * Allocation barrier

use motoko_rts_macros::ic_mem_fn;

use crate::{
    memory::Memory,
    types::{is_skewed, Value},
};

#[no_mangle]
pub unsafe extern "C" fn running_gc() -> bool {
    false
}

/// Write a potential pointer value with a pre-update barrier and resolving pointer forwarding.
/// Used for the incremental GC.
/// `location` (unskewed) denotes the field or array element where the value will be written to.
/// `value` (skewed if a pointer) denotes the value that will be written to the location.
/// The barrier can be conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub unsafe fn write_with_barrier<M: Memory>(_mem: &mut M, location: *mut Value, value: Value) {
    debug_assert!(!is_skewed(location as u32));
    debug_assert_ne!(location, core::ptr::null_mut());

    *location = value;
}

/// Allocation barrier to be called after a new object allocation.
/// The new object needs to be fully initialized, except for the payload of a blob.
/// Used for the incremental GC.
#[no_mangle]
pub unsafe extern "C" fn allocation_barrier(_new_object: Value) {}
