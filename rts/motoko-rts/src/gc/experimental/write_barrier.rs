//! Write barrier, used for experimental GC

use super::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::types::Value;
use motoko_rts_macros::ic_mem_fn;

pub static mut REMEMBERED_SET: Option<RememberedSet> = None;

/// Create an new remembered set. Necessary to activate the write barrier for experimental GC.
#[ic_mem_fn]
pub unsafe fn create_remembered_set<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
}

/// Write barrier, used for experimental GC. Called before the actual write.
/// `location`: updated location of pointer (address of object field or array element).
///
/// As the barrier is called before the write, `*location` still refers to the old value.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(mem: &mut M, location: u32) {
    match &mut REMEMBERED_SET {
        None => return,
        Some(remembered_set) => {
            //println!(100, "Write barrier {:#x}", location);

            // Make sure we unskewed the object when calculating the field
            debug_assert_eq!(location & 0b1, 0);
            remembered_set.insert(mem, Value::from_raw(location));
        }
    }
}
