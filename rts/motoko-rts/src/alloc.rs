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
use crate::types::{size_of, Array, Blob, Bytes, SkewedPtr, Words, TAG_ARRAY, TAG_BLOB};

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

#[no_mangle]
pub(crate) unsafe extern "C" fn alloc_blob(size: Bytes<u32>) -> SkewedPtr {
    // NOTE: alloc_bytes rounds the size up to the next word and allocates words, but we initialize
    // blob length as `size` instead of `round_up_to_word(size)`. This is fine as as GC knows that
    // we can only allocate words and looks for objects in word boundaries.
    let ptr = alloc_bytes(size_of::<Blob>().to_bytes() + size);
    let blob = ptr.unskew() as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).len = size;
    ptr
}
