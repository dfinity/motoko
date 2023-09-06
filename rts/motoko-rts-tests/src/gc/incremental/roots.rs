use std::{array::from_fn, mem::size_of, ptr::null_mut};

use motoko_rts::{
    gc::incremental::roots::{visit_roots, Roots},
    types::{Array, Value},
};

use crate::gc::{
    heap::MotokoHeap,
    utils::{ObjectIdx, GC, WORD_SIZE},
};

pub unsafe fn test() {
    println!("  Testing roots...");

    let object_map: [(ObjectIdx, Vec<ObjectIdx>); 10] = from_fn(|id| (id as u32, vec![]));
    let root_ids = [2, 4, 6, 8];
    let continuation_ids = [3, 5, 7];
    let region0_ptr = [0];

    let heap = MotokoHeap::new(
        &object_map,
        &root_ids,
        &continuation_ids,
        &region0_ptr,
        GC::Incremental,
    );
    check_visit_static_roots(&heap, &root_ids);
    check_visit_continuation_table(&heap, &continuation_ids);
}

unsafe fn check_visit_static_roots(heap: &MotokoHeap, root_ids: &[ObjectIdx]) {
    let roots = get_roots(heap);
    let mut visited_static_roots = vec![];
    visit_roots(
        roots,
        heap.heap_base_address(),
        &mut visited_static_roots,
        |context, field| {
            let object = *field;
            let array = object.as_array();
            if array.len() == 1 {
                let id = object_id(&heap, array as usize);
                context.push(id);
            }
        },
    );
    assert_eq!(visited_static_roots, root_ids);
}

unsafe fn check_visit_continuation_table(heap: &MotokoHeap, continuation_ids: &[ObjectIdx]) {
    let roots = get_roots(heap);
    let mut visited_continuations = vec![];
    visit_roots(
        roots,
        heap.heap_base_address(),
        &mut visited_continuations,
        |context, field| {
            let object = *field;
            let array = object.as_array();
            if array.len() != 1 {
                assert_eq!(context.len(), 0);
                for index in 0..array.len() {
                    let element = array.get(index);
                    let id = object_id(&heap, element.get_ptr());
                    context.push(id);
                }
            }
        },
    );
    assert_eq!(visited_continuations, continuation_ids);
}

unsafe fn get_roots(heap: &MotokoHeap) -> Roots {
    let static_roots = Value::from_ptr(heap.static_root_array_address());
    let continuation_table_location = heap.continuation_table_ptr_address() as *mut Value;
    let region0_ptr_location = heap.region0_ptr_location() as *mut Value;
    assert_ne!(continuation_table_location, null_mut());
    Roots {
        static_roots,
        continuation_table_location,
        region0_ptr_location,
    }
}

fn object_id(heap: &MotokoHeap, address: usize) -> u32 {
    let offset = address - heap.heap_base_address();
    const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;
    assert_eq!(OBJECT_SIZE, 16);
    assert_eq!(offset % OBJECT_SIZE, 0);
    (offset / OBJECT_SIZE) as u32
}
