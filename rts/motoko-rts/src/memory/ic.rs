// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::constants::{GB, WASM_PAGE_SIZE};
use crate::rts_trap_with;
use crate::types::{Bytes, Value, Words};
use core::arch::wasm64;

// TODO: Can we inspect the maximum main memory limit from the IC?
pub const MAIN_MEMORY_LIMIT: Bytes<usize> = Bytes(400 * GB);

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        crate::gc::incremental::get_partitioned_heap().allocate(self, n)
    }

    /// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
    /// Ensure a memory reserve of at least one Wasm page depending on the canister state.
    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: usize) {
        const LAST_PAGE_LIMIT: usize = 0xFFFF_FFFF_FFFF_0000;
        debug_assert_eq!(LAST_PAGE_LIMIT, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
        // Spare a memory reserve for use by the GC.
        let memory_reserve = crate::gc::incremental::memory_reserve();
        // In any case, the last Wasm memory page is reserved to guard against shadow call stack overflows.
        // This call stack is used both by the Rust runtime system implementation and by the compiler backend,
        // see module `Stack` in `compile.ml`. This requires function activation frames to be less than the
        // Wasm page size.
        debug_assert!(memory_reserve <= LAST_PAGE_LIMIT);
        let limit = LAST_PAGE_LIMIT - memory_reserve;
        // The pointer is one byte larger than the memory size to be allocated, see the comment above.
        if ptr > limit {
            rts_trap_with("Cannot grow memory")
        };
        let page_size = WASM_PAGE_SIZE.as_usize();
        let total_pages_needed = ((ptr + page_size - 1) / page_size) as usize;
        let current_pages = wasm64::memory_size(0);
        if total_pages_needed > current_pages {
            if wasm64::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
                // replica signals that there is not enough memory
                rts_trap_with("Cannot grow memory");
            }
        }
    }
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<usize> {
    crate::gc::incremental::get_partitioned_heap().reclaimed_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<usize> {
    get_heap_size() + get_reclaimed()
}

#[no_mangle]
pub unsafe extern "C" fn get_heap_size() -> Bytes<usize> {
    crate::gc::incremental::get_partitioned_heap().occupied_size()
}

#[no_mangle]
pub unsafe extern "C" fn get_max_live_size() -> Bytes<usize> {
    crate::gc::incremental::get_max_live_size()
}
