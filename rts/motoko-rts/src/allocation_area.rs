//! Defines mutator allocation interface

use crate::space::Space;
use crate::types::{SkewedPtr, Words};

// TODO: We should use `MaybeUninit` here, but `MaybeUninit::assume_init_ref` does not exist in the
// Rust version we're using.. Need to update rustc.
static mut ALLOCATION_AREA: Option<Space> = None;

pub unsafe fn init() {
    ALLOCATION_AREA = Some(Space::new());
}

#[no_mangle]
pub(crate) unsafe fn alloc_words(n: Words<u32>) -> SkewedPtr {
    ALLOCATION_AREA.as_mut().unwrap().alloc_words(n)
}

pub(crate) unsafe fn free_and_update_allocation_area(new: Space) {
    let old = ALLOCATION_AREA.replace(new).unwrap();
    old.free();
}
