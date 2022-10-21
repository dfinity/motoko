//! Write barrier
//! Pre-update, object-level barrier for incremental snapshot-at-the-beginning marking.

pub mod remembered_set;
#[cfg(debug_assertions)]
pub mod sanity_checks;

use crate::{memory::Memory, types::Value};

use motoko_rts_macros::ic_mem_fn;

/// (Re-)initialize the write barrier.
#[ic_mem_fn(ic_only)]
pub unsafe fn init_write_barrier<M: Memory>(_mem: &mut M) {
    #[cfg(debug_assertions)]
    sanity_checks::init_write_barrier(_mem);
}

/// Write barrier to be called BEFORE a pointer store.
/// `object` (skewed) denotes the containing object wherein a field or array element is written.
/// The barrier is conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(_mem: &mut M, _object: Value) {
    debug_assert!(_object.is_ptr());
    #[cfg(debug_assertions)]
    if (_object.get_ptr() as *mut Value) != core::ptr::null_mut() {
        debug_assert!(
            _object.tag() >= crate::types::TAG_OBJECT && _object.tag() <= crate::types::TAG_NULL
        );
    }
    #[cfg(debug_assertions)]
    sanity_checks::record_write(_mem, _object);
}
