// This module is only enabled when compiling the RTS for IC or WASI.

#[non_incremental_gc]
pub mod linear_memory;

#[incremental_gc]
pub mod partitioned_memory;

use motoko_rts_macros::{
    classical_persistence, enhanced_orthogonal_persistence, incremental_gc, non_incremental_gc,
};

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;
use crate::types::Bytes;

// Provided by generated code
extern "C" {
    #[classical_persistence]
    fn get_heap_base() -> usize;

    #[classical_persistence]
    pub(crate) fn get_static_roots() -> crate::types::Value;

    fn keep_memory_reserve() -> bool;
}

#[classical_persistence]
pub(crate) unsafe fn get_aligned_heap_base() -> usize {
    // align to 32 bytes
    ((get_heap_base() + 31) / 32) * 32
}

#[enhanced_orthogonal_persistence]
pub(crate) unsafe fn get_aligned_heap_base() -> usize {
    crate::persistence::HEAP_START
}

#[classical_persistence]
pub const MAIN_MEMORY_LIMIT: Bytes<usize> = Bytes(usize::MAX);

#[enhanced_orthogonal_persistence]
pub const MAIN_MEMORY_LIMIT: Bytes<usize> = Bytes(400 * crate::constants::GB);

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

#[enhanced_orthogonal_persistence]
pub fn wasm_memory_size() -> usize {
    core::arch::wasm64::memory_size(0)
}

#[classical_persistence]
pub fn wasm_memory_size() -> usize {
    core::arch::wasm32::memory_size(0)
}

#[enhanced_orthogonal_persistence]
pub fn wasm_memory_grow(pages: usize) -> usize {
    core::arch::wasm64::memory_grow(0, pages)
}

#[classical_persistence]
pub fn wasm_memory_grow(pages: usize) -> usize {
    core::arch::wasm32::memory_grow(0, pages)
}

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
/// Ensure a memory reserve of at least one Wasm page depending on the canister state.
/// `memory_reserve`: A memory reserve in bytes ensured during update and initialization calls.
//  For use by queries and upgrade calls. The reserve may vary depending on the GC and the phase of the GC.
unsafe fn grow_memory(ptr: u64, memory_reserve: usize) {
    const LAST_PAGE_LIMIT: usize = usize::MAX - WASM_PAGE_SIZE.as_usize() + 1;
    debug_assert!(memory_reserve <= MAIN_MEMORY_LIMIT.as_usize());
    let limit = if keep_memory_reserve() {
        // Spare a memory reserve during update and initialization calls for use by queries and upgrades.
        MAIN_MEMORY_LIMIT.as_usize() - memory_reserve
    } else {
        // Spare the last Wasm memory page on queries and upgrades to support the Rust call stack boundary checks.
        LAST_PAGE_LIMIT
    };
    if ptr > limit as u64 {
        rts_trap_with("Cannot grow memory")
    };
    let page_size = WASM_PAGE_SIZE.as_usize() as u64;
    let total_pages_needed = ((ptr + page_size - 1) / page_size) as usize;
    let current_pages = wasm_memory_size();
    if total_pages_needed > current_pages {
        if wasm_memory_grow(total_pages_needed - current_pages) == core::usize::MAX {
            // replica signals that there is not enough memory
            rts_trap_with("Cannot grow memory");
        }
    }
}
