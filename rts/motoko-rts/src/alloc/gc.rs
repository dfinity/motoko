//! Implements allocation routines used by the generated code and the GC.

use core::arch::wasm32;

use crate::constants::WASM_PAGE_SIZE;
use crate::gc;
use crate::rts_trap_with;
use crate::types::{skew, Bytes, SkewedPtr, Words};

#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> SkewedPtr {
    let bytes = n.to_bytes();
    // Update ALLOCATED
    gc::ALLOCATED += Bytes(bytes.0 as u64);

    // Update heap pointer
    let old_hp = gc::HP;
    let new_hp = old_hp + bytes.0;
    gc::HP = new_hp;

    // Grow memory if needed
    grow_memory(new_hp as usize);

    skew(old_hp as usize)
}

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
pub(crate) unsafe fn grow_memory(ptr: usize) {
    let page_size = u64::from(WASM_PAGE_SIZE.0);
    let total_pages_needed = (((ptr as u64) + page_size - 1) / page_size) as usize;
    let current_pages = wasm32::memory_size(0);
    if total_pages_needed > current_pages {
        if wasm32::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
            rts_trap_with("Cannot grow memory");
        }
    }
}
