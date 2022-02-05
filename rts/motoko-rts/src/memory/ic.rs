// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::rts_trap_with;
use crate::types::*;

use core::arch::wasm32;

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations
pub(crate) static mut ALLOCATED: Bytes<u64> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: u32 = 0;

// Provided by generated code
extern "C" {
    pub(crate) fn get_heap_base() -> u32;
    pub(crate) fn get_static_roots() -> Value;
}

pub(crate) unsafe fn get_aligned_heap_base() -> u32 {
    // align to 32 bytes
    ((get_heap_base() + 31) / 32) * 32
}

#[no_mangle]
unsafe extern "C" fn init(align: bool) {
    HP = if align {
        get_aligned_heap_base()
    } else {
        get_heap_base()
    };
    LAST_HP = HP;
    // make sure that the page HP points to is allocated too
    grow_memory(u64::from(HP))
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
    Bytes(HP - get_aligned_heap_base())
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();
        // Update ALLOCATED
        let delta = u64::from(bytes.as_u32());
        ALLOCATED += Bytes(delta);

        // Update heap pointer
        let old_hp = u64::from(HP);
        let new_hp = old_hp + delta;

        // Grow memory if needed
        if (old_hp ^ new_hp) >> 16 != 0 {
            grow_memory(new_hp);
            debug_assert!(new_hp <= u64::from(core::u32::MAX));
        }

        HP = new_hp as u32;

        Value::from_ptr(old_hp as usize)
    }
}

/// Page allocation. Ensures that the memory up to, and including, the given pointer is allocated,
/// with the slight exception of not allocating the extra page for 0x100_000_000. For all
/// `ptr > 0x100_000_000` the call to `wasm32::memory_grow` will trap.
/// Invariant: ptr must be a reasonable pointer (i.e. less than 0x200_000_000).
#[inline(never)]
unsafe fn grow_memory(ptr: u64) {
    debug_assert!(ptr < 2 * u64::from(core::u32::MAX));
    debug_assert_eq!(2 * u64::from(WASM_PAGE_SIZE.as_u32()) >> 16, 2);
    // Note: the `+ 1` below makes sure that there is a page allocated
    //       for ptr (even when it points exactly to its start) and
    //       the correction `ptr >> 32` is a branchless way to undo this
    //       behaviour when ptr points beyond the first 4 GiB of memory.
    let total_pages_needed = (ptr >> 16) as usize + 1 - (ptr >> 32) as usize;
    let current_pages = wasm32::memory_size(0);
    if total_pages_needed > current_pages {
        if wasm32::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
            rts_trap_with("Cannot grow memory");
        }
    }
}
