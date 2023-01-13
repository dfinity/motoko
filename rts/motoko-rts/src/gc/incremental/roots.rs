use crate::types::Value;

/// GC root set.
#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `visit_roots()`.
}

#[cfg(feature = "ic")]
pub unsafe fn root_set() -> Roots {
    use crate::memory::ic;
    Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *crate::continuation_table::continuation_table_loc(),
    }
}

pub unsafe fn visit_roots<C, V: Fn(&mut C, Value)>(
    roots: Roots,
    heap_base: usize,
    context: &mut C,
    visit_object: V,
) {
    visit_static_roots(roots.static_roots, heap_base, context, &visit_object);
    visit_continuation_table(roots.continuation_table, context, &visit_object);
}

unsafe fn visit_static_roots<C, V: Fn(&mut C, Value)>(
    static_roots: Value,
    heap_base: usize,
    context: &mut C,
    visit_object: &V,
) {
    let root_array = static_roots.as_array();
    for index in 0..root_array.len() {
        let mutbox = root_array.get(index).as_mutbox();
        debug_assert!((mutbox as usize) < heap_base);
        let value = (*mutbox).field;
        if value.is_ptr() && value.get_ptr() >= heap_base {
            visit_object(context, value);
        }
    }
}

unsafe fn visit_continuation_table<C, V: Fn(&mut C, Value)>(
    continuation_table: Value,
    context: &mut C,
    visit_object: &V,
) {
    if continuation_table.is_ptr() {
        visit_object(context, continuation_table);
    }
}
