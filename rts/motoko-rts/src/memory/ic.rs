// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::page_alloc::alloc_page;
use crate::types::*;

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations
pub(crate) static mut ALLOCATED: Bytes<u64> = Bytes(0);

/// Heap pointer in the current allocation area
pub(crate) static mut HP: u32 = 0;

/// Heap pointer after last GC
// FIXME: Won't be correct with the page allocator
pub(crate) static mut LAST_HP: u32 = 0;

// Provided by generated code
extern "C" {
    pub(crate) fn get_heap_base() -> u32;
    pub(crate) fn get_static_roots() -> SkewedPtr;
}

#[no_mangle]
unsafe extern "C" fn init() {
    HP = alloc_page().start() as u32;
    LAST_HP = HP;
}

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[no_mangle]
unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    ALLOCATED
}

#[no_mangle]
unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    // FIXME: With the page allocator this is no longer correct
    Bytes(HP - get_heap_base())
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        /*
        let bytes = n.to_bytes();

        ALLOCATED += Bytes(bytes.0 as u64);

        let allocation_area = address_to_page(HP as usize);

        let alloc = if (HP + bytes.0) as usize >= allocation_area.end() {
            let alloc = alloc_page().start() as u32;
            HP = alloc + bytes.0;
            alloc
        } else {
            HP + bytes.0
        };

        skew(alloc as usize)
        */
        todo!()
    }
}
