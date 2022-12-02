//! Write barrier, used for generational GC

use super::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;

#[cfg(feature = "ic")]
/// (Re-)initialize the write barrier for generational GC.
pub(crate) unsafe fn init_post_write_barrier<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
}

/// Write barrier to be called AFTER the pointer store, used for generational GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn post_write_barrier<M: Memory>(mem: &mut M, location: u32) {
    // Must be an unskewed address.
    debug_assert_eq!(location & 0b1, 0);
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if location < mem.last_heap_pointer() {
        // Nested ifs are more efficient when counting instructions on IC (explicit return counts as an instruction).
        let value = *(location as *mut Value);
        if value.points_to_or_beyond(mem.last_heap_pointer() as usize) {
            #[allow(clippy::collapsible_if)]
            if location >= mem.heap_base() {
                // Trap pointers that lead from old generation (or static roots) to young generation.
                REMEMBERED_SET
                    .as_mut()
                    .unwrap()
                    .insert(mem, Value::from_raw(location));
            }
        }
    }
}
