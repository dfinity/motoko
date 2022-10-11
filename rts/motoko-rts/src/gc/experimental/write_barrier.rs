//! Write barrier, used for experimental GC

use super::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;
pub static mut LAST_HP: u32 = 0;

/// (Re-)initialize the write barrier for experimental GC.
#[ic_mem_fn(ic_only)]
pub unsafe fn init_write_barrier<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
    LAST_HP = crate::memory::ic::LAST_HP;
}

/// Write barrier to be called AFTER the pointer store, used for experimental GC.
/// `location`: location of modified pointer (address of object field or array element).
///
/// As the barrier is called after the write, `*location` refers to the NEW value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(mem: &mut M, location: u32) {
    debug_assert_eq!(location & 0b1, 0); // must be unskewed address
    match &mut REMEMBERED_SET {
        None => return,
        Some(remembered_set) => {
            if location < LAST_HP {
                let value = *(location as *mut Value);
                if value.is_ptr() && value.get_raw() >= LAST_HP {
                    // trap pointers that lead from old generation (or static roots) to young generation
                    //println!(100, "Write barrier {:#x}", location);
                    remembered_set.insert(mem, Value::from_raw(location));
                }
            }
        }
    }
}
