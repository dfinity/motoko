//! Write barrier

pub mod remembered_set;
#[cfg(debug_assertions)]
pub mod sanity_checks;

use crate::memory::Memory;

use self::remembered_set::RememberedSet;

use motoko_rts_macros::ic_mem_fn;

// Note: Remembered set will be collected by GC.
pub static mut REMEMBERED_SET: Option<RememberedSet> = None;

/// (Re-)initialize the write barrier.
#[ic_mem_fn(ic_only)]
pub unsafe fn init_write_barrier<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
}

/// Write barrier to be called BEFORE the pointer store.
/// No effect is the write barrier is deactivated.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(_mem: &mut M, location: u32) {
    debug_assert_eq!(location & 0b1, 0); // must be unskewed address
    #[cfg(debug_assertions)]
    match &mut REMEMBERED_SET {
        None => return,
        Some(remembered_set) => {
            remembered_set.insert(_mem, crate::types::Value::from_raw(location));
        }
    }
}
