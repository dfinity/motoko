//! Defines mutator allocation interface

use crate::page_alloc::ic::IcPageAlloc;
use crate::rts_trap_with;
use crate::space::Space;
use crate::types::{size_of, Array, Blob, Bytes, Value, Words, TAG_ARRAY, TAG_BLOB};

// TODO: We should use `MaybeUninit` here, but `MaybeUninit::assume_init_ref` does not exist in the
// Rust version we're using.. Need to update rustc.
pub static mut ALLOCATION_SPACE: Option<Space<IcPageAlloc>> = None;

pub unsafe fn init() {
    ALLOCATION_SPACE = Some(Space::new(IcPageAlloc {}));
}

#[no_mangle]
pub unsafe fn alloc_words(n: Words<u32>) -> Value {
    ALLOCATION_SPACE.as_mut().unwrap().alloc_words(n)
}

/// Helper for allocating blobs
#[no_mangle]
pub unsafe fn alloc_blob(size: Bytes<u32>) -> Value {
    let ptr = alloc_words(size_of::<Blob>() + size.to_words());
    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = ptr.get_ptr() as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).len = size;
    ptr
}

/// Helper for allocating arrays
#[no_mangle]
pub unsafe fn alloc_array(len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > 1 << (32 - 2 - 1) {
        // 2 for word size, 1 to divide by two
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.get_ptr() as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).len = len;

    skewed_ptr
}

pub unsafe fn free_and_update_allocation_space(new: Space<IcPageAlloc>) {
    let mut old = ALLOCATION_SPACE.replace(new).unwrap();
    old.free();
}
