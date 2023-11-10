use core::arch::wasm32;

use super::{get_aligned_heap_base, IcMemory, Memory};
use crate::{constants::WORD_SIZE, types::*};

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<u64> = Bytes(0);

// Heap pointer (skewed)
extern "C" {
    fn setHP(new_hp: usize);
    fn getHP() -> u32;
}

pub(crate) unsafe fn set_hp_unskewed(new_hp: usize) {
    setHP(skew(new_hp))
}
pub(crate) unsafe fn get_hp_unskewed() -> usize {
    unskew(getHP() as usize)
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
pub unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    Bytes((get_hp_unskewed() - get_aligned_heap_base()) as u32)
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();
        let delta = u64::from(bytes.as_u32());

        // Update heap pointer
        let old_hp = u64::from(getHP());
        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > ((wasm32::memory_size(0) as u64) << 16) {
            self.grow_memory(new_hp)
        }

        debug_assert!(new_hp <= u64::from(core::u32::MAX));
        setHP(new_hp as usize);

        Value::from_raw(old_hp as u32)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: u64) {
        super::grow_memory(ptr);
    }
}

pub(super) unsafe fn resize_heap(new_size: usize) {
    assert_eq!(new_size % WORD_SIZE as usize, 0);
    let new_hp = get_aligned_heap_base() + new_size;
    set_hp_unskewed(new_hp);
}
