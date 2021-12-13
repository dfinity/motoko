//! Defines mutator allocation interface

use crate::page_alloc::ic::IcPageAlloc;
use crate::space::Space;
use crate::types::{Bytes, Value, Words};

use core::mem::{replace, MaybeUninit};

pub static mut ALLOCATION_SPACE: MaybeUninit<Space<IcPageAlloc>> = MaybeUninit::uninit();

pub unsafe fn init() {
    ALLOCATION_SPACE = MaybeUninit::new(Space::new(IcPageAlloc {}));
}

#[no_mangle]
pub unsafe fn alloc_words(n: Words<u32>) -> Value {
    ALLOCATION_SPACE.assume_init_mut().alloc_words(n)
}

/// Helper for allocating blobs
#[no_mangle]
pub unsafe fn alloc_blob(size: Bytes<u32>) -> Value {
    ALLOCATION_SPACE.assume_init_mut().alloc_blob(size)
}

/// Helper for allocating arrays
#[no_mangle]
pub unsafe fn alloc_array(len: u32) -> Value {
    ALLOCATION_SPACE.assume_init_mut().alloc_array(len)
}

pub unsafe fn free_and_update_allocation_space(new: Space<IcPageAlloc>) {
    let mut old = replace(ALLOCATION_SPACE.assume_init_mut(), new);
    old.free();
}
