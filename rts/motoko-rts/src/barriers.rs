use motoko_rts_macros::{incremental_gc, non_incremental_gc};

use crate::{memory::Memory, types::Value};

#[incremental_gc]
pub unsafe fn init_with_barrier<M: Memory>(_mem: &mut M, location: *mut Value, value: Value) {
    *location = value.forward_if_possible();
}

#[non_incremental_gc]
pub unsafe fn init_with_barrier<M: Memory>(mem: &mut M, location: *mut Value, value: Value) {
    *location = value;
    crate::gc::generational::write_barrier::post_write_barrier(mem, location as usize);
}

#[incremental_gc]
pub unsafe fn write_with_barrier<M: Memory>(mem: &mut M, location: *mut Value, value: Value) {
    crate::gc::incremental::barriers::write_with_barrier(mem, location, value);
}

#[non_incremental_gc]
pub unsafe fn write_with_barrier<M: Memory>(mem: &mut M, location: *mut Value, value: Value) {
    *location = value;
    crate::gc::generational::write_barrier::post_write_barrier(mem, location as usize);
}

#[incremental_gc]
pub unsafe fn allocation_barrier(new_object: Value) -> Value {
    crate::gc::incremental::barriers::allocation_barrier(new_object)
}

#[non_incremental_gc]
pub unsafe fn allocation_barrier(new_object: Value) -> Value {
    new_object
}
