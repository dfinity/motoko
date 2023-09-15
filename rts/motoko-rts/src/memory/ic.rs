// This module is only enabled when compiling the RTS for IC or WASI.

#[non_incremental_gc]
pub mod linear_memory;
#[incremental_gc]
pub mod partitioned_memory;

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;
use crate::types::{Bytes, Value};
use core::arch::wasm64;
use motoko_rts_macros::*;

// Provided by generated code
extern "C" {
    fn get_heap_base() -> usize;
    pub(crate) fn get_static_roots() -> Value;
}

pub(crate) unsafe fn get_aligned_heap_base() -> usize {
    // Required for the mark bitmap in the compacting GC.
    const BYTE_ALIGNMENT: usize = usize::BITS as usize;
    ((get_heap_base() + (BYTE_ALIGNMENT - 1)) / BYTE_ALIGNMENT) * BYTE_ALIGNMENT
}

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<usize> = Bytes(0);

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<usize> {
    MAX_LIVE
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated,
/// with the slight exception of not allocating the extra page for address 0xFFFF_FFFF_FFFF_0000.
unsafe fn grow_memory(ptr: usize) {
    const MEMORY_LIMIT: usize = 0xFFFF_FFFF_FFFF_0000;
    debug_assert_eq!(MEMORY_LIMIT, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
    if ptr > MEMORY_LIMIT {
        // spare the last wasm memory page
        rts_trap_with("Cannot grow memory")
    };
    let page_size = WASM_PAGE_SIZE.as_usize();
    let total_pages_needed = (ptr + page_size - 1) / page_size;
    let current_pages = wasm64::memory_size(0);
    if total_pages_needed > current_pages {
        if wasm64::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
            // replica signals that there is not enough memory
            rts_trap_with("Cannot grow memory");
        }
    }
}
