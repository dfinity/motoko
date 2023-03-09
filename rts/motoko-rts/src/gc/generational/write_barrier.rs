//! Write barrier, used for generational GC

use crate::memory::Memory;
use crate::remembered_set::RememberedSet;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;

/// (Re-)initialize the write barrier for generational GC.
#[cfg(feature = "ic")]
pub(super) unsafe fn init_generational_write_barrier<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
}

pub unsafe fn using_generational_barrier() -> bool {
    REMEMBERED_SET.is_some()
}

/// Write barrier to be called AFTER the pointer store, used for generational GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn generational_write_barrier<M: Memory>(mem: &mut M, location: usize) {
    // Must be an unskewed address.
    debug_assert_eq!(location & 0b1, 0);
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if location < mem.get_last_heap_pointer() {
        // Nested ifs are more efficient when counting instructions on IC (explicit return counts as an instruction).
        let value = *(location as *mut Value);
        if value.points_to_or_beyond(mem.get_last_heap_pointer()) {
            if location >= mem.get_heap_base() {
                // Trap pointers that lead from old generation (or static roots) to young generation.
                REMEMBERED_SET
                    .as_mut()
                    .unwrap()
                    .insert(mem, Value::from_raw(location as u32));
            }
        }
    }
}
