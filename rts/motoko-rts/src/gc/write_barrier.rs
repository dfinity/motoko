//! Write barrier

pub mod remembered_set;
#[cfg(debug_assertions)]
pub mod sanity_checks;

use core::{ptr::null_mut, mem::size_of};

use crate::{memory::Memory, types::{Value, TAG_OBJECT, TAG_NULL, object_size, Obj}};

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
/// `object` (skewed) is the containing object wherein the location is written.
/// `field_address` (unskewed) is the address of the written field or array element within the `object`.
#[ic_mem_fn]
pub unsafe fn write_barrier<M: Memory>(_mem: &mut M, object: Value, field_address: u32) {
    debug_assert!(object.is_ptr());
    debug_assert_eq!(field_address & 0b1, 0);
    if (object.get_ptr() as *mut Value) != null_mut() {
        let tag = object.tag();
        debug_assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        let size = object_size(object.get_ptr()).to_bytes().as_usize();
        debug_assert!(field_address as usize >= object.get_ptr() + size_of::<Obj>() && field_address as usize <= object.get_ptr() + size);
    }
    #[cfg(debug_assertions)]
    match &mut REMEMBERED_SET {
        None => return,
        Some(remembered_set) => {
            remembered_set.insert(_mem, Value::from_raw(field_address));
        }
    }
}
