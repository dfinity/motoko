// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;
use crate::types::{Bytes, Value, Words};
use core::arch::wasm32;

extern "C" {
    fn keep_memory_reserve() -> bool;
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        crate::gc::incremental::get_partitioned_heap().allocate(self, n)
    }

    /// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
    /// Ensure a memory reserve for the incremental GC and an additional reserve for upgrades and queries.
    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: u64) {
        const LAST_PAGE_LIMIT: usize = 0xFFFF_0000;
        debug_assert_eq!(LAST_PAGE_LIMIT, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
        let limit = if keep_memory_reserve() {
            let memory_reserve = crate::gc::incremental::memory_reserve();
            // Spare a memory reserve during update and initialization calls for use by queries and upgrades.
            usize::MAX - memory_reserve + 1
        } else {
            // Spare the last Wasm memory page on queries and upgrades to support the Rust call stack boundary checks.
            LAST_PAGE_LIMIT
        };
        if ptr > limit as u64 {
            rts_trap_with("Cannot grow memory")
        };
        let page_size = u64::from(WASM_PAGE_SIZE.as_u32());
        let total_pages_needed = ((ptr + page_size - 1) / page_size) as usize;
        let current_pages = wasm32::memory_size(0);
        if total_pages_needed > current_pages {
            if wasm32::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
                // replica signals that there is not enough memory
                rts_trap_with("Cannot grow memory");
            }
        }
    }
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    crate::gc::incremental::get_partitioned_heap().reclaimed_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    Bytes(u64::from(get_heap_size().as_u32())) + get_reclaimed()
}

#[no_mangle]
pub unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    crate::gc::incremental::get_partitioned_heap().occupied_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    crate::gc::incremental::get_max_live_size()
}
