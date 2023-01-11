//! Write barrier for the incremental GC.
//! Pre-update, field-level barrier used for snapshot-at-the-beginning marking.

use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::incremental::IncrementalGC,
    memory::Memory,
    types::{is_skewed, Value},
};

/// Write a potential pointer value with a pre-update barrier and adjusting the written forwarded. Used for the incremental GC.
/// `location` (unskewed) denotes the field or array element where the value will be written to.
/// `value` (skewed if a pointer) denotes the value that will be written to the location.
/// The barrier can be conservatively called even if the stored value might not be a pointer.
/// Additional write effects:
/// * Pre-update barrier: Used during the GC mark phase to guarantee snapshot-at-the-beginning marking.
/// * Resolve forwarding: Used during the GC update phase to adjust old pointer values to the new forwarded address.
#[ic_mem_fn]
pub unsafe fn incremental_write_with_barrier<M: Memory>(
    mem: &mut M,
    location: *mut Value,
    value: Value,
) {
    incremental_pre_write_barrier(mem, location);
    *location = value.forward_if_possible();
}

/// Write barrier to be called BEFORE a pointer store. Used for the incremental GC.
/// `location` (unskewed) denotes the target address of the write, the address of a field or an array element.
/// The barrier is conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub(crate) unsafe fn incremental_pre_write_barrier<M: Memory>(mem: &mut M, location: *mut Value) {
    debug_assert!(!is_skewed(location as u32));
    debug_assert_ne!(location, core::ptr::null_mut());
    IncrementalGC::instance(mem).pre_write_barrier(*location);
}
