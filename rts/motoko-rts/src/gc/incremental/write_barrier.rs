//! Write barrier, used by the incremental GC

use crate::memory::Memory;
use crate::remembered_set::RememberedSet;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut YOUNG_REMEMBERED_SET: Option<RememberedSet> = None;
// TODO: Adjust also this `HEAP_BASE` when object table grows/shrinks! Better incorporate into mem.
pub static mut HEAP_BASE: u32 = 0;
pub static mut LAST_HP: u32 = 0;

/// Activate the write barrier for the incremental GC.
#[cfg(feature = "ic")]
pub(super) unsafe fn init_incremental_write_barrier<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    HEAP_BASE = ic::HEAP_BASE;
    new_young_remembered_set(mem, ic::LAST_HP as usize);
}

/// Create a new young remembered set after a young generation collection.
pub(super) unsafe fn new_young_remembered_set<M: Memory>(mem: &mut M, last_hp: usize) {
    assert!(YOUNG_REMEMBERED_SET.is_none());
    YOUNG_REMEMBERED_SET = Some(RememberedSet::new(mem));
    LAST_HP = last_hp as u32;
}

/// Write barrier to be called AFTER the pointer store, used by the incremental GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn incremental_write_barrier<M: Memory>(mem: &mut M, location: u32) {
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
                // Note: We could also only record the target value, as no threading is performed.
                // However, the location can be overwritten, such that the target object may still become garbage.
                // Therefore, this allows objects to be collected even if they have only been temporarily referenced
                // from the old generation.
                YOUNG_REMEMBERED_SET
                    .as_mut()
                    .unwrap()
                    .insert(mem, Value::from_raw(location));
            }
        }
    }
}
