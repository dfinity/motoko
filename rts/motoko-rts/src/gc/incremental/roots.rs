use crate::{
    memory::Roots, remembered_set::RememberedSet, types::Value, visitor::points_to_or_beyond,
};

pub unsafe fn visit_roots<C, V: Fn(&mut C, Value)>(
    roots: Roots,
    generation_base: usize,
    remembered_set: Option<&RememberedSet>,
    context: &mut C,
    visit_value: V,
) {
    visit_static_roots(roots.static_roots, generation_base, context, &visit_value);
    visit_continuation_table(
        roots.continuation_table_location,
        generation_base,
        context,
        &visit_value,
    );
    if remembered_set.is_some() {
        visit_young_remembered_set(
            remembered_set.unwrap(),
            generation_base,
            context,
            &visit_value,
        );
    }
}

unsafe fn visit_static_roots<C, V: Fn(&mut C, Value)>(
    static_roots: Value,
    generation_base: usize,
    context: &mut C,
    visit_value: &V,
) {
    let root_array = static_roots.as_array();
    for index in 0..root_array.len() {
        let mutbox = root_array.get(index).as_mutbox();
        debug_assert!((mutbox as usize) < generation_base);
        let field = &mut (*mutbox).field;
        if points_to_or_beyond(field, generation_base) {
            visit_value(context, *field);
        }
    }
}

unsafe fn visit_continuation_table<C, V: Fn(&mut C, Value)>(
    continuation_table_location: *mut Value,
    generation_base: usize,
    context: &mut C,
    visit_value: &V,
) {
    if points_to_or_beyond(continuation_table_location, generation_base) {
        visit_value(context, *continuation_table_location);
    }
}

unsafe fn visit_young_remembered_set<C, V: Fn(&mut C, Value)>(
    remembered_set: &RememberedSet,
    generation_base: usize,
    context: &mut C,
    visit_value: &V,
) {
    let mut iterator = remembered_set.iterate();
    while iterator.has_next() {
        let value = iterator.current();
        debug_assert!(value.points_to_or_beyond(generation_base));
        visit_value(context, value);
        iterator.next();
    }
}
