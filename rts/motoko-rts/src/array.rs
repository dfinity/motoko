use crate::common::rts_trap_with;
use crate::types::{skew, Array, SkewedPtr};

/// Returns address of Nth payload field of an array
#[no_mangle]
pub unsafe extern "C" fn array_field_addr(array: SkewedPtr, idx: u32) -> SkewedPtr {
    let array_ptr = array.unskew() as *const Array;

    if idx >= (*array_ptr).len {
        rts_trap_with("Array index out of bounds\0".as_ptr());
    }

    let payload_begin = array_ptr.offset(1) as *const u32;
    skew(payload_begin.offset(idx as isize) as usize)
}

/// Index an array. Does not check bounds.
pub unsafe fn array_idx_unchecked(array_ptr: *const Array, idx: u32) -> SkewedPtr {
    let payload_begin = array_ptr.offset(1) as *const u32;
    SkewedPtr(*payload_begin.offset(idx as isize) as usize)
}
