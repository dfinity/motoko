use core::arch::wasm64;

use super::{get_aligned_heap_base, IcMemory, Memory};
use crate::types::*;

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<usize> = Bytes(0);

// Heap pointer (skewed)
extern "C" {
    fn setHP(new_hp: usize);
    fn getHP() -> usize;
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
unsafe extern "C" fn get_reclaimed() -> Bytes<usize> {
    RECLAIMED
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<usize> {
    get_heap_size() + get_reclaimed()
}

#[no_mangle]
pub unsafe extern "C" fn get_heap_size() -> Bytes<usize> {
    Bytes(get_hp_unskewed() - get_aligned_heap_base())
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        let bytes = n.to_bytes();
        let delta = bytes.as_usize();

        // Update heap pointer
        let old_hp = getHP();
        if old_hp >= usize::MAX - delta {
            panic!("Out of memory");
        }

        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > (wasm64::memory_size(0) << 16) {
            self.grow_memory(new_hp)
        }

        setHP(new_hp);

        Value::from_raw(old_hp)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: usize) {
        super::grow_memory(ptr);
    }
}
