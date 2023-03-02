//! Write barrier, used for generational GC

use super::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;
pub static mut HEAP_BASE: u32 = 0;
pub static mut LAST_HP: u32 = 0;

/// (Re-)initialize the write barrier for generational GC.
#[ic_mem_fn(ic_only)]
pub(super) unsafe fn init_write_barrier<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    REMEMBERED_SET = Some(RememberedSet::new(mem));
    HEAP_BASE = ic::HEAP_BASE;
    LAST_HP = ic::LAST_HP;
}

/// Write barrier to be called AFTER the pointer store, used for generational GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn generational_write_barrier<M: Memory>(mem: &mut M, location: u32) {
    // Must be an unskewed address.
    debug_assert_eq!(location & 0b1, 0);
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if location < LAST_HP {
        // Nested ifs are more efficient when counting instructions on IC (explicit return counts as an instruction).
        let value = *(location as *mut Value);
        if value.points_to_or_beyond(LAST_HP as usize) {
            if location >= HEAP_BASE {
                // Trap pointers that lead from old generation (or static roots) to young generation.
                REMEMBERED_SET
                    .as_mut()
                    .unwrap()
                    .insert(mem, Value::from_raw(location));
            }
        }
    }
}
