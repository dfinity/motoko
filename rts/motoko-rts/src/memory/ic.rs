// This module is only enabled when compiling the RTS for IC or WASI.

use motoko_rts_macros::ic_mem_fn;

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

// For giving the other logic the right number at the right time.
pub(crate) static mut REGION_SET_MEM_SIZE: Option<u64> = None;

// For giving the other logic the right number at the right time.
pub(crate) static mut REGION_MEM_SIZE_INIT: bool = false;

// Mirrored field from stable memory, for handling upgrade logic.
pub(crate) static mut REGION_TOTAL_ALLOCATED_BLOCKS: u16 = 0;

// Region 0 -- classic API for stable memory, as a dedicated region.
// pub(crate) static mut REGION_0: Value = Value::from_ptr(0);
pub(crate) static mut REGION_0: Value = Value::from_scalar(0);

// Region 1 -- reserved for reclaimed regions' blocks (to do).
// pub(crate) static mut REGION_1: Value = Value::from_ptr(0);
pub(crate) static mut REGION_1: Value = Value::from_scalar(0);

/// TEMP -- for logging during testing.
pub(crate) static mut NEXT_REGION_LOG_ID: u16 = 0;

// Provided by generated code
extern "C" {
    fn get_heap_base() -> u32;
    pub(crate) fn get_static_roots() -> Value;
}

pub(crate) unsafe fn get_aligned_heap_base() -> u32 {
    // align to 32 bytes
    ((get_heap_base() + 31) / 32) * 32
}

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

#[ic_mem_fn]
pub unsafe fn region_init<M: Memory>(mem: &mut M) {
    crate::region::region_init_(mem);
}

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated,
/// with the slight exception of not allocating the extra page for address 0xFFFF_0000.
unsafe fn grow_memory(ptr: u64) {
    debug_assert_eq!(0xFFFF_0000, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
    if ptr > 0xFFFF_0000 {
        // spare the last wasm memory page
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
