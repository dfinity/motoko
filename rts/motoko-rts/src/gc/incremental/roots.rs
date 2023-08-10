use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::Value, visitor::pointer_to_dynamic_heap};

use super::barriers::write_with_barrier;

/// Root referring to all canister variables.
/// This root is reinitialized on each canister upgrade.
/// The scalar sentinel denotes an uninitialized root.
static mut STATIC_ROOT: Value = Value::from_scalar(0);

/// GC root set.
pub type Roots = [*mut Value; 3];

#[cfg(feature = "ic")]
pub unsafe fn root_set() -> Roots {
    use crate::{continuation_table::continuation_table_loc, persistence::stable_actor_location};
    [
        static_root_location(),
        continuation_table_loc(),
        stable_actor_location(),
    ]
}

pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    heap_base: usize,
    context: &mut C,
    visit_field: V,
) {
    for location in roots {
        // TODO: Check whether all pointers lead to dynamic heap, no static heap
        if pointer_to_dynamic_heap(location, heap_base) {
            visit_field(context, location);
        }
    }
}

unsafe fn static_root_location() -> *mut Value {
    &mut STATIC_ROOT as *mut Value
}

#[ic_mem_fn]
pub unsafe fn set_static_root<M: Memory>(mem: &mut M, value: Value) {
    let location = &mut STATIC_ROOT as *mut Value;
    write_with_barrier(mem, location, value);
}

#[no_mangle]
pub unsafe extern "C" fn get_static_root() -> Value {
    assert!(STATIC_ROOT.is_ptr());
    STATIC_ROOT
}
