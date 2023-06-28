use core::arch::wasm64;

use super::{get_aligned_heap_base, IcMemory, Memory};
use crate::types::*;

/// Amount of garbage collected so far.
pub(crate) static mut RECLAIMED: Bytes<usize> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: usize = 0;

/// Heap pointer after last GC
pub(crate) static mut LAST_HP: usize = 0;

pub(crate) unsafe fn initialize() {
    HP = get_aligned_heap_base();
    LAST_HP = HP;
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
    Bytes(HP - get_aligned_heap_base())
}

impl Memory for IcMemory {
    #[inline]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        let bytes = n.to_bytes();
        let delta = bytes.as_usize();

        // Update heap pointer
        let old_hp = HP;
        if old_hp > usize::MAX - delta {
            panic!("Out of memory");
        }

        let new_hp = old_hp + delta;

        // Grow memory if needed
        if new_hp > (wasm64::memory_size(0) << 16) {
            self.grow_memory(new_hp)
        }

        HP = new_hp;

        Value::from_ptr(old_hp)
    }

    #[inline(never)]
    unsafe fn grow_memory(&mut self, ptr: usize) {
        super::grow_memory(ptr);
    }
}
