// This module is only enabled when compiling the RTS for IC or WASI.

use super::Memory;
use crate::constants::WASM_PAGE_SIZE;
use crate::gc::incremental::object_table::ObjectTable;
use crate::rts_trap_with;
use crate::types::*;

use core::arch::wasm32;

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Heap base (start of dynamic heap space)
pub(crate) static mut HEAP_BASE: u32 = 0;

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: u32 = 0;

// Provided by generated code
extern "C" {
    pub(crate) fn get_static_roots() -> Value;
}

fn align_to_32_bytes(address: u32) -> u32 {
    ((address + 31) / 32) * 32
}

pub(crate) unsafe fn initialize_memory<M: Memory>(
    mem: &mut M,
    heap_base: u32,
    use_object_table: bool,
) {
    HEAP_BASE = align_to_32_bytes(heap_base);
    HP = HEAP_BASE;
    LAST_HP = HP;
    if use_object_table {
        initalize_object_table(mem);
    }
}

unsafe fn initalize_object_table<M: Memory>(mem: &mut M) {
    const INITIAL_TABLE_LENGTH: usize = 10_000;
    let size = Words(INITIAL_TABLE_LENGTH as u32);
    assert_eq!(HEAP_BASE, HP);
    let base = mem.alloc_words(size) as *mut usize;
    let table = ObjectTable::new(base, INITIAL_TABLE_LENGTH);
    HEAP_BASE = align_to_32_bytes(HP);
    HP = HEAP_BASE;
    if LAST_HP < HEAP_BASE {
        LAST_HP = HEAP_BASE;
    }
    OBJECT_TABLE = Some(table);
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
    Bytes(u64::from(get_heap_size().as_u32())) + RECLAIMED
}

#[no_mangle]
unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    Bytes(HP - HEAP_BASE)
}

/// Provides a `Memory` implementation, to be used in functions compiled for IC or WASI. The
/// `Memory` implementation allocates in Wasm heap with Wasm `memory.grow` instruction.
pub struct IcMemory;

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize {
        let bytes = n.to_bytes();
        let delta = u64::from(bytes.as_u32());

        // Update heap pointer
        let old_hp = u64::from(HP);
        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > ((wasm32::memory_size(0) as u64) << 16) {
            grow_memory(new_hp)
        }

        HP = new_hp as u32;

        old_hp as usize
    }
}

/// Page allocation. Ensures that the memory up to, but excluding, the given pointer is allocated,
/// with the slight exception of not allocating the extra page for address 0xFFFF_0000.
#[inline(never)]
unsafe fn grow_memory(ptr: u64) {
    debug_assert_eq!(0xFFFF_0000, usize::MAX - WASM_PAGE_SIZE.as_usize() + 1);
    if ptr > 0xFFFF_0000 {
        // spare the last wasm memory page
        rts_trap_with("Cannot allocate memory")
    };
    let page_size = u64::from(WASM_PAGE_SIZE.as_u32());
    let total_pages_needed = ((ptr + page_size - 1) / page_size) as usize;
    let current_pages = wasm32::memory_size(0);
    if total_pages_needed > current_pages {
        if wasm32::memory_grow(0, total_pages_needed - current_pages) == core::usize::MAX {
            // replica signals that there is not enough memory
            rts_trap_with("Cannot grow memory");
        }
        debug_assert!(wasm32::memory_size(0) <= 65535)
    }
}
