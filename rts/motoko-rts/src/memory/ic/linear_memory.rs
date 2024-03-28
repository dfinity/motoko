use core::arch::wasm32;

use super::{get_aligned_heap_base, IcMemory, Memory};
use crate::{memory::GENERAL_MEMORY_RESERVE, types::*};

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Maximum live data retained in a GC.
pub(crate) static mut MAX_LIVE: Bytes<u32> = Bytes(0);

// Heap pointer (skewed)
extern "C" {
    fn setHP(new_hp: usize);
    fn getHP() -> usize;
}

pub(crate) unsafe fn set_hp_unskewed(new_hp: usize) {
    setHP(skew(new_hp))
}
pub(crate) unsafe fn get_hp_unskewed() -> usize {
    unskew(getHP())
}

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: usize = 0;

pub(crate) unsafe fn initialize() {
    LAST_HP = get_aligned_heap_base();
    set_hp_unskewed(LAST_HP);
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
pub unsafe extern "C" fn get_heap_size() -> Bytes<usize> {
    Bytes(get_hp_unskewed() - get_aligned_heap_base())
}

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<usize> {
    MAX_LIVE
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        let bytes = n.to_bytes();
        let delta = u64::from(bytes.as_usize());

        // Update heap pointer
        let old_hp = u64::from(getHP());
        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > ((wasm32::memory_size(0) as u64) << 16) {
            self.grow_memory(new_hp)
        }

        debug_assert!(new_hp <= u64::from(core::usize::MAX));
        setHP(new_hp);

        Value::from_raw(old_hp as usize)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: u64) {
        super::grow_memory(ptr, GENERAL_MEMORY_RESERVE);
    }
}
