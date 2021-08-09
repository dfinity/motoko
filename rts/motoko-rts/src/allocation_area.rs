//! Defines mutator allocation interface

use crate::page_alloc::ic::IcPageAlloc;
use crate::space::Space;
use crate::types::{SkewedPtr, Words};

// TODO: We should use `MaybeUninit` here, but `MaybeUninit::assume_init_ref` does not exist in the
// Rust version we're using.. Need to update rustc.
pub static mut ALLOCATION_AREA: Option<Space<IcPageAlloc>> = None;

pub unsafe fn init() {
    ALLOCATION_AREA = Some(Space::new(IcPageAlloc {}));
}

#[no_mangle]
pub unsafe fn alloc_words(n: Words<u32>) -> SkewedPtr {
    ALLOCATION_AREA.as_mut().unwrap().alloc_words(n)
}

pub unsafe fn free_and_update_allocation_area(new: Space<IcPageAlloc>) {
    let mut old = ALLOCATION_AREA.replace(new).unwrap();
    old.free();
}
