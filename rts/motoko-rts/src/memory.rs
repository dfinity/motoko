#[cfg(feature = "ic")]
pub mod ic;

use crate::constants::WASM_HEAP_SIZE;
use crate::rts_trap_with;
use crate::types::*;
use motoko_rts_macros::{export, ic_only, testing_only};

#[ic_only]
#[inline]
pub unsafe fn alloc_words(n: Words<u32>) -> Value {
    ic::alloc_words(n)
}

#[testing_only]
extern "C" {
    /// Implemented externall in the RTS unit tests.
    /// Peformance optimization: Avoiding a trait for this dependency injection.
    pub fn alloc_words(n: Words<u32>) -> Value;
}

/// Helper for allocating blobs
#[export]
pub unsafe fn alloc_blob(size: Bytes<u32>) -> Value {
    let ptr = alloc_words(size_of::<Blob>() + size.to_words());
    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = ptr.get_ptr() as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).len = size;
    ptr
}

/// Helper for allocating arrays
#[export]
pub unsafe fn alloc_array(len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > (WASM_HEAP_SIZE / 2).0 {
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.get_ptr() as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).len = len;

    skewed_ptr
}
