// This module is only enabled when compiling the RTS for IC or WASI.

use crate::constants::WASM_PAGE_SIZE;
use crate::gc::incremental::get_partitioned_heap;
use crate::rts_trap_with;
use crate::types::*;

use core::arch::wasm32;

use motoko_rts_macros::export;

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: u32 = 0;

#[derive(PartialEq)]
pub(crate) enum HeapLayout {
    /// Linear heap. Used by the copying, compacting, and generational GC.
    Linear,
    /// Partitioned heap. Used by the incremental GC.
    Partitioned,
}

static mut LAYOUT: HeapLayout = HeapLayout::Linear;

// Provided by generated code
extern "C" {
    fn get_heap_base() -> u32;
    pub(crate) fn get_static_roots() -> Value;
}

pub(crate) unsafe fn get_aligned_heap_base() -> u32 {
    // align to 32 bytes
    ((get_heap_base() + 31) / 32) * 32
}

pub(crate) unsafe fn initialize_memory(heap_layout: HeapLayout) {
    HP = get_aligned_heap_base();
    LAST_HP = HP;
    LAYOUT = heap_layout;
}

#[export]
unsafe fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

#[export]
unsafe fn get_reclaimed() -> Bytes<u64> {
    match LAYOUT {
        HeapLayout::Partitioned => get_partitioned_heap().reclaimed_size(),
        HeapLayout::Linear => RECLAIMED,
    }
}

#[export]
pub unsafe fn get_total_allocations() -> Bytes<u64> {
    Bytes(u64::from(get_heap_size().as_u32())) + get_reclaimed()
}

#[export]
pub unsafe fn get_heap_size() -> Bytes<u32> {
    match LAYOUT {
        HeapLayout::Partitioned => get_partitioned_heap().occupied_size(),
        HeapLayout::Linear => Bytes(HP - get_aligned_heap_base()),
    }
}

#[inline]
pub unsafe fn alloc_words(n: Words<u32>) -> Value {
    match LAYOUT {
        HeapLayout::Partitioned => get_partitioned_heap().allocate(n),
        HeapLayout::Linear => linear_alloc_words(n),
    }
}

#[inline]
pub unsafe fn linear_alloc_words(n: Words<u32>) -> Value {
    let bytes = n.to_bytes();
    let delta = u64::from(bytes.as_u32());

    // Update heap pointer
    let old_hp = u64::from(HP);
    let new_hp = old_hp + delta;

    // Grow memory if needed
    if new_hp > ((wasm32::memory_size(0) as u64) << 16) {
        grow_memory(new_hp)
    }

    debug_assert!(new_hp <= u64::from(core::u32::MAX));
    HP = new_hp as u32;

    Value::from_ptr(old_hp as usize)
}

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated,
/// with the slight exception of not allocating the extra page for address 0xFFFF_0000.
#[inline(never)]
pub unsafe fn grow_memory(ptr: u64) {
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
