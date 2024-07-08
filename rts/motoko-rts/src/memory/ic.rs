// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::constants::{GB, WASM_PAGE_SIZE};
use crate::rts_trap_with;
use crate::types::{Bytes, Value, Words};
use core::arch::wasm64;
use core::cmp::max;

extern "C" {
    fn keep_memory_reserve() -> bool;
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

/// Probe the memory capacity beyond this limit by pre-allocating the reserve.
/// This is necessary because there is no function to query the IC's main memory capacity for 64-bit.
/// It is assumed that the capacity is at least the 32-bit space of 4GB, minus the last reserved
/// Wasm page (for Rust call stack overflow detection, see `compile.ml`).
const GUARANTEED_MEMORY_CAPACITY: usize = 4 * GB - WASM_PAGE_SIZE.0;

/// Assumption (not correctness-critical): The IC offers main memory in multiples of 2 GB.
/// This helps to avoid overly frequent memory probing when the heap grows.
/// The capacity granularity only serves as a heuristics for GC scheduling.
const IC_MEMORY_CAPACITY_GRANULARITY: usize = 2 * GB;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        crate::gc::incremental::get_partitioned_heap().allocate(self, n)
    }

    /// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated.
    /// Ensure a memory reserve of at least one Wasm page depending on the canister state.
    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: usize) {
        let reserve = memory_reserve();
        debug_assert!(reserve <= GUARANTEED_MEMORY_CAPACITY);
        let memory_demand = if keep_memory_reserve() && ptr > GUARANTEED_MEMORY_CAPACITY - reserve {
            // Detect overflow of `ptr + reserve`.
            if ptr > usize::MAX - reserve {
                rts_trap_with("Cannot grow memory");
            }
            // The reserve will be pre-allocated as a way to check the main memory capacity.
            // As the reserve can be relatively large for small heaps, this is only done when
            // the memory demand exceeds `GUARANTEED_MEMORY_CAPACITY`.
            ptr + reserve
        } else {
            // Either no reserve is needed or there is enough guaranteed memory capacity for the reserve,
            // such that we can skip the pre-allocation of a reserve.
            ptr
        };
        allocate_wasm_memory(Bytes(memory_demand));
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

/// Supposed minimum memory capacity used for GC scheduling heuristics.
/// The result may increase after time. This is because the actual capacity is
/// not known in advance and can only be derived by memory allocation probing.
/// Moreover, the IC may increase the canister main memory capacity in newer versions.
pub(crate) fn minimum_memory_capacity() -> Bytes<usize> {
    let allocated_memory = wasm64::memory_size(0) * WASM_PAGE_SIZE.as_usize();
    let unrounded_capacity = max(allocated_memory, GUARANTEED_MEMORY_CAPACITY);
    Bytes(unrounded_capacity).next_multiple_of(IC_MEMORY_CAPACITY_GRANULARITY)
}

/// Grow memory without memory reserve. Used during RTS initialization and by the ordinary
/// reserve-conscious memory-grow operation (`Memory::grow_memory`).
pub(crate) unsafe fn allocate_wasm_memory(memory_size: Bytes<usize>) {
    const LAST_PAGE_LIMIT: usize = 0xFFFF_FFFF_FFFF_0000;
    debug_assert_eq!(LAST_PAGE_LIMIT, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
    // Never allocate the last page (Rust call stack overflow detection, see `compile.ml`).
    if memory_size.as_usize() > LAST_PAGE_LIMIT {
        rts_trap_with("Cannot grow memory");
    }
    if !probe_wasm_memory(memory_size) {
        // replica signals that there is not enough memory
        rts_trap_with("Cannot grow memory");
    }
}

/// Try to allocate an amount of Wasm memory by growing the Wasm memory space if needed.
/// Returns true if the memory has been allocated and is available.
/// Otherwise, it returns false if there does not exist enough Wasm memory.
pub(crate) unsafe fn probe_wasm_memory(memory_size: Bytes<usize>) -> bool {
    let page_size = WASM_PAGE_SIZE.as_usize();
    let total_pages_needed = (memory_size.as_usize() + page_size - 1) / page_size;
    let current_pages = wasm64::memory_size(0);
    if total_pages_needed > current_pages {
        wasm64::memory_grow(0, total_pages_needed - current_pages) != core::usize::MAX
    } else {
        true
    }
}
