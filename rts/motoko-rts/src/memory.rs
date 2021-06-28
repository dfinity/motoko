#[cfg(feature = "ic")]
pub mod ic;

use crate::rts_trap_with;
use crate::types::*;

use motoko_rts_macros::ic_mem_fn;

pub trait Memory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr;
}

#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, size: Bytes<u32>) -> SkewedPtr {
    let ptr = mem.alloc_words(size_of::<Blob>() + size.to_words());
    let blob = ptr.unskew() as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).len = size;
    ptr
}

#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, len: u32) -> SkewedPtr {
    // Array payload should not be larger than half of the memory
    if len > 1 << (32 - 2 - 1) {
        // 2 for word size, 1 to divide by two
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = mem.alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.unskew() as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).len = len;

    skewed_ptr
}
