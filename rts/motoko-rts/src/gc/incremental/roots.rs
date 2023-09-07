use motoko_rts_macros::ic_mem_fn;

use crate::{types::Value, visitor::is_pointer_field};

/// Root referring to all canister variables.
/// This root is reinitialized on each canister upgrade.
/// The scalar sentinel denotes an uninitialized root.
#[cfg(feature = "ic")]
static mut STATIC_ROOT: Value = Value::from_scalar(0);

/// GC root set.
pub type Roots = [*mut Value; 6];

#[cfg(feature = "ic")]
pub unsafe fn root_set() -> Roots {
    use crate::{
        continuation_table::continuation_table_loc,
        persistence::{null_singleton_location, stable_actor_location, stable_type_location},
        region::region0_get_ptr_loc,
    };
    [
        static_root_location(),
        continuation_table_loc(),
        stable_actor_location(),
        stable_type_location(),
        null_singleton_location(),
        region0_get_ptr_loc()
    ]
}

pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    context: &mut C,
    visit_field: V,
) {
    for location in roots {
        if is_pointer_field(location) {
            visit_field(context, location);
        }
    }
}

#[cfg(feature = "ic")]
unsafe fn static_root_location() -> *mut Value {
    &mut STATIC_ROOT as *mut Value
}

#[ic_mem_fn(ic_only)]
pub unsafe fn set_static_root<M: crate::memory::Memory>(mem: &mut M, value: Value) {
    use super::barriers::write_with_barrier;

    let location = &mut STATIC_ROOT as *mut Value;
    write_with_barrier(mem, location, value);
}

#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe extern "C" fn get_static_root() -> Value {
    assert!(STATIC_ROOT.is_ptr());
    STATIC_ROOT
}

unsafe fn visit_region0_ptr<C, V: Fn(&mut C, *mut Value)>(
    region0_ptr_location: *mut Value,
    heap_base: usize,
    context: &mut C,
    visit_field: &V,
) {
    if pointer_to_dynamic_heap(region0_ptr_location, heap_base) {
        visit_field(context, region0_ptr_location);
    }
}
