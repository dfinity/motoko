#[cfg(feature = "gc")]
#[path = "alloc/gc.rs"]
pub(crate) mod alloc_impl;

#[cfg(not(feature = "gc"))]
#[path = "alloc/nogc.rs"]
mod alloc_impl;

pub use alloc_impl::alloc_words;

#[cfg(feature = "gc")]
pub(crate) use alloc_impl::grow_memory;

use crate::rts_trap_with;
use crate::types::{size_of, Array, Bytes, SkewedPtr, Words, TAG_ARRAY};

#[no_mangle]
pub unsafe extern "C" fn alloc_bytes(n: Bytes<u32>) -> SkewedPtr {
    alloc_words(n.to_words())
}

#[no_mangle]
pub unsafe extern "C" fn alloc_array(len: u32) -> SkewedPtr {
    // Array payload should not be larger than half of the memory
    if len > 1 << (32 - 2 - 1) {
        // 2 for word size, 1 to divide by two
        rts_trap_with("Array allocation too large\0".as_ptr());
    }

    let skewed_ptr = alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.unskew() as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).len = len;

    skewed_ptr
}
