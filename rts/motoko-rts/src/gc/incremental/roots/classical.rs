use crate::types::Value;
use crate::visitor::classical::pointer_to_dynamic_heap;

#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_location: *mut Value,
    pub region0_ptr_location: *mut Value,
    // If new roots are added in future, extend `visit_roots()`.
}

#[cfg(feature = "ic")]
pub unsafe fn root_set() -> Roots {
    use crate::memory::ic;
    Roots {
        static_roots: ic::get_static_roots(),
        continuation_table_location: crate::continuation_table::continuation_table_loc(),
        region0_ptr_location: crate::region::region0_get_ptr_loc(),
    }
}

pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    heap_base: usize,
    context: &mut C,
    visit_field: V,
) {
    visit_static_roots(roots.static_roots, heap_base, context, &visit_field);
    visit_continuation_table(
        roots.continuation_table_location,
        heap_base,
        context,
        &visit_field,
    );
    visit_region0_ptr(roots.region0_ptr_location, heap_base, context, &visit_field);
}

unsafe fn visit_static_roots<C, V: Fn(&mut C, *mut Value)>(
    static_roots: Value,
    heap_base: usize,
    context: &mut C,
    visit_field: &V,
) {
    let root_array = static_roots.as_array();
    for index in 0..root_array.len() {
        let mutbox = root_array.get(index).as_mutbox();
        debug_assert!((mutbox as usize) < heap_base);
        let field = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field, heap_base) {
            visit_field(context, field);
        }
    }
}

unsafe fn visit_continuation_table<C, V: Fn(&mut C, *mut Value)>(
    continuation_table_location: *mut Value,
    heap_base: usize,
    context: &mut C,
    visit_field: &V,
) {
    if pointer_to_dynamic_heap(continuation_table_location, heap_base) {
        visit_field(context, continuation_table_location);
    }
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
