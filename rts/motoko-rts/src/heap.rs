#[cfg(feature = "ic")]
pub mod ic;

use crate::rts_trap_with;
use crate::types::*;

pub trait Heap {
    /// Get address of the beginning of dynamic heap
    unsafe fn get_heap_base(&mut self) -> u32;

    unsafe fn get_hp(&mut self) -> u32;

    unsafe fn set_hp(&mut self, hp: u32);

    /// Get pointer to the static memory with an array to the static roots
    unsafe fn get_static_roots(&mut self) -> SkewedPtr;

    unsafe fn get_closure_table_loc(&mut self) -> *mut SkewedPtr;

    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr;

    /// Ensure that the memory up to the given pointer is allocated
    unsafe fn grow_memory(&mut self, ptr: usize);

    unsafe fn note_live_size(&mut self, live_size: Bytes<u32>);

    unsafe fn note_reclaimed(&mut self, reclaimed: Bytes<u32>);

    unsafe fn alloc_blob(&mut self, size: Bytes<u32>) -> SkewedPtr {
        let ptr = self.alloc_words(size_of::<Blob>() + size.to_words());
        let blob = ptr.unskew() as *mut Blob;
        (*blob).header.tag = TAG_BLOB;
        (*blob).len = size;
        ptr
    }

    unsafe fn alloc_array(&mut self, len: u32) -> SkewedPtr {
        // Array payload should not be larger than half of the memory
        if len > 1 << (32 - 2 - 1) {
            // 2 for word size, 1 to divide by two
            rts_trap_with("Array allocation too large");
        }

        let skewed_ptr = self.alloc_words(size_of::<Array>() + Words(len));

        let ptr: *mut Array = skewed_ptr.unskew() as *mut Array;
        (*ptr).header.tag = TAG_ARRAY;
        (*ptr).len = len;

        skewed_ptr
    }
}
