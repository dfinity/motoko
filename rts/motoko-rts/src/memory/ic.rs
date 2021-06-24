use super::Memory;
use crate::rts_trap_with;
use crate::types::*;

use motoko_rts_macros::ic_fn;

use core::arch::wasm32;

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations
pub(crate) static mut ALLOCATED: Bytes<u64> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

// Provided by generated code
extern "C" {
    pub(crate) fn get_mem_base() -> u32;
    pub(crate) fn get_static_roots() -> SkewedPtr;
}

#[ic_fn]
unsafe fn init() {
    HP = get_mem_base() as u32;
}

#[ic_fn]
unsafe fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

#[ic_fn]
unsafe fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[ic_fn]
unsafe fn get_total_allocations() -> Bytes<u64> {
    ALLOCATED
}

#[ic_fn]
unsafe fn get_mem_size() -> Bytes<u32> {
    Bytes(HP - get_mem_base())
}

pub struct IcMemory;

impl Memory for IcMemory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        let bytes = n.to_bytes();
        // Update ALLOCATED
        ALLOCATED += Bytes(bytes.0 as u64);

        // Update mem pointer
        let old_hp = HP;
        let new_hp = old_hp + bytes.0;
        HP = new_hp;

        // Grow memory if needed
        grow_memory(new_hp as usize);

        skew(old_hp as usize)
    }

    unsafe fn get_hp(&self) -> usize {
        HP as usize
    }
}

unsafe fn grow_memory(ptr: usize) {
    let total_pages_needed = ((ptr / 65536) + 1) as i32;
    let current_pages = wasm32::memory_size(0) as i32;
    let new_pages_needed = total_pages_needed - current_pages;
    if new_pages_needed > 0 {
        if wasm32::memory_grow(0, new_pages_needed as usize) == core::usize::MAX {
            rts_trap_with("Cannot grow memory");
        }
    }
}
