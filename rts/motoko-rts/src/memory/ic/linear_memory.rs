use core::arch::wasm32;

use super::{get_aligned_heap_base, IcMemory, Memory};
use crate::types::*;

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: u32 = 0;

pub(crate) unsafe fn initialize() {
    HP = get_aligned_heap_base();
    LAST_HP = HP;
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    Bytes(u64::from(get_heap_size().as_u32())) + get_reclaimed()
}

#[no_mangle]
pub unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    Bytes(HP - get_aligned_heap_base())
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();
        let delta = u64::from(bytes.as_u32());

        // Update heap pointer
        let old_hp = u64::from(HP);
        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > ((wasm32::memory_size(0) as u64) << 16) {
            self.grow_memory(new_hp)
        }

        debug_assert!(new_hp <= u64::from(core::u32::MAX));
        HP = new_hp as u32;

        Value::from_ptr(old_hp as usize)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: u64) {
        super::grow_memory(ptr);
    }
}
