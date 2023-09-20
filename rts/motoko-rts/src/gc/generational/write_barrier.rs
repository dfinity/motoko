//! Write barrier, used for generational GC

use crate::gc::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;
pub static mut HEAP_BASE: usize = 0;
pub static mut LAST_HP: usize = 0;

#[cfg(feature = "ic")]
/// (Re-)initialize the write barrier for generational GC.
pub(crate) unsafe fn init_generational_write_barrier<M: Memory>(mem: &mut M) {
    use crate::memory::ic::{self, linear_memory};
    REMEMBERED_SET = Some(RememberedSet::new(mem));
    HEAP_BASE = ic::get_aligned_heap_base();
    LAST_HP = linear_memory::LAST_HP;
}

/// Write barrier to be called AFTER the pointer store, used for the generational GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect if the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn post_write_barrier<M: Memory>(mem: &mut M, location: usize) {
    // Must be an unskewed address.
    debug_assert_eq!(location & 0b1, 0);
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if location < LAST_HP {
        // Nested ifs are more efficient when counting instructions on IC (explicit return counts as an instruction).
        let value = *(location as *mut Value);
        if value.points_to_or_beyond(LAST_HP) {
            #[allow(clippy::collapsible_if)]
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
