//! Write barrier for the incremental GC.
//! Pre-update, field-level barrier used for snapshot-at-the-beginning marking.

use crate::{memory::Memory, types::Value, gc::incremental::{increment, MARK_STACK, mark_potential_object}};

use motoko_rts_macros::ic_mem_fn;

/// Write barrier to be called BEFORE a pointer store.
/// `location` (unskewed) denotes the field or array element that will be written.
/// The barrier is conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(mem: &mut M, location: *mut Value) {
    debug_assert!(location as usize & 0b1 == 0);
    debug_assert!(location != core::ptr::null_mut());
    // TODO: Optimize to exclude irrelevant values as fast as possible
    if MARK_STACK.is_some() {
        mark_potential_object(mem, *location);
    }
    increment(mem);
}
