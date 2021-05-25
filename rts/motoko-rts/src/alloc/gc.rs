//! Implements allocation routines used by the generated code and the GC.

use core::alloc::{GlobalAlloc, Layout};
use core::arch::wasm32;

use crate::gc;
use crate::rts_trap_with;
use crate::types::{skew, Bytes, SkewedPtr, Words};

static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> SkewedPtr {
    let bytes = n.to_bytes();
    let ptr = ALLOC.alloc(Layout::from_size_align_unchecked(bytes.0 as usize, 0)) as usize;
    skew(ptr)
}

/*
/// Page allocation. Ensures that the memory up to the given pointer is allocated.
pub(crate) unsafe fn grow_memory(ptr: usize) {
    let total_pages_needed = ((ptr / 65536) + 1) as i32;
    let current_pages = wasm32::memory_size(0) as i32;
    let new_pages_needed = total_pages_needed - current_pages;
    if new_pages_needed > 0 {
        if wasm32::memory_grow(0, new_pages_needed as usize) == core::usize::MAX {
            rts_trap_with("Cannot grow memory");
        }
    }
}
*/
