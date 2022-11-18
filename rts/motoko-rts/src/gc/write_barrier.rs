//! Write barrier
//! Pre-update, field-level barrier for incremental snapshot-at-the-beginning marking.

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
/// `location` (unskewed) denotes the field or array element that will be written.
/// The barrier is conservatively called even if the stored value might not be a pointer.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(_mem: &mut M, _location: *mut Value) {
    debug_assert!(_location as usize & 0b1 == 0);
    debug_assert!(_location != core::ptr::null_mut());
    #[cfg(debug_assertions)]
    {
        let value = *_location;
        if value.is_ptr() && (value.get_ptr() as *mut Value) != core::ptr::null_mut() {
            debug_assert!(
                value.tag() >= crate::types::TAG_OBJECT && value.tag() <= crate::types::TAG_NULL
            );
        }
    }
    #[cfg(debug_assertions)]
    sanity_checks::record_write(_mem, Value::from_raw(_location as u32));
}
