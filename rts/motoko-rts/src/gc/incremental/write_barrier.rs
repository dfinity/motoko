//! Write barrier for the incremental GC.
//! Pre-update, field-level barrier used for snapshot-at-the-beginning marking.

use motoko_rts_macros::ic_mem_fn;

use crate::{gc::incremental::IncrementalGC, memory::Memory, types::Value};

/// Write barrier to be called BEFORE a pointer store.
/// `location` (unskewed) denotes the field or array element that will be written.
/// The barrier is conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub unsafe fn pre_write_barrier<M: Memory>(mem: &mut M, location: *mut Value) {
    debug_assert!(location as usize & 0b1 == 0);
    debug_assert!(location != core::ptr::null_mut());
    IncrementalGC::pre_write_barrier(mem, *location);
}
