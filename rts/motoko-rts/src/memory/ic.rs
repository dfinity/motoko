// This module is only enabled when compiling the RTS for IC or WASI.

#[non_incremental_gc]
pub mod linear_memory;
#[incremental_gc]
pub mod partitioned_memory;

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;
use crate::types::{Bytes, Value};
use core::arch::wasm32;
use motoko_rts_macros::*;

// Provided by generated code
extern "C" {
    fn get_heap_base() -> usize;
    pub(crate) fn get_static_roots() -> Value;
    fn keep_memory_reserve() -> bool;
}

pub(crate) unsafe fn get_aligned_heap_base() -> usize {
    // align to 32 bytes
    ((get_heap_base() + 31) / 32) * 32
}

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
/// Ensure a memory reserve of at least one Wasm page depending on the canister state.
/// `memory_reserve`: A memory reserve in bytes ensured during update and initialization calls.
//  For use by queries and upgrade calls. The reserve may vary depending on the GC and the phase of the GC.
unsafe fn grow_memory(ptr: u64, memory_reserve: usize) {
    const LAST_PAGE_LIMIT: usize = 0xFFFF_0000;
    debug_assert_eq!(LAST_PAGE_LIMIT, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
    let limit = if keep_memory_reserve() {
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
