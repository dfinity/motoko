use crate::types::Value;

/// GC root set.
#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_location: *mut Value,
    // If new roots are added in future, extend `visit_roots()`.
}

#[cfg(feature = "ic")]
pub unsafe fn root_set() -> Roots {
    use crate::memory::ic;
    Roots {
        static_roots: ic::get_static_roots(),
        continuation_table_location: crate::continuation_table::continuation_table_loc(),
    }
}

pub unsafe fn visit_roots<C, V: Fn(&mut C, *mut Value)>(
    roots: Roots,
    heap_base: usize,
    context: &mut C,
    visit_field: V,
) {
    visit_static_roots(roots.static_roots, heap_base, context, &visit_field);
    visit_field(context, roots.continuation_table_location);
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
        visit_field(context, &mut (*mutbox).field);
    }
}
