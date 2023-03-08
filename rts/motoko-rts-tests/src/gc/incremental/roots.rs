use std::{array::from_fn, mem::size_of, ptr::null_mut};

use motoko_rts::{
    gc::common::Roots,
    gc::incremental::roots::visit_roots,
    remembered_set::{RememberedSet, INITIAL_TABLE_LENGTH},
    types::{Array, Obj, Value, Words, OBJECT_TABLE},
};

use crate::{
    gc::{
        heap::MotokoHeap,
        utils::{ObjectIdx, GC, WORD_SIZE},
    },
    memory::TestMemory,
};

pub unsafe fn test() {
    println!("  Testing roots...");

    check_regular_roots();
    check_visit_remembered_set();
}

unsafe fn check_regular_roots() {
    let object_map: [(ObjectIdx, Vec<ObjectIdx>); 16] = from_fn(|id| (id as u32, vec![]));
    let root_indices = [2, 4, 6, 8, 10, 12, 14];
    let continuation_indices = [3, 5, 11, 13];

    let heap = MotokoHeap::new(
        &object_map,
        &root_indices,
        &continuation_indices,
        GC::Incremental,
    );
    let mut mem = TestMemory::new(Words(WORD_SIZE as u32 * INITIAL_TABLE_LENGTH));
    check_visit_static_roots(&heap, &root_indices);
    check_visit_continuation_table(&heap, &continuation_indices);
    OBJECT_TABLE = None;
}

unsafe fn check_visit_static_roots(heap: &MotokoHeap, root_indices: &[ObjectIdx]) {
    let roots = get_roots(heap);
    let mut visited_static_roots = vec![];
    visit_roots(
        roots,
        heap.heap_base_address(),
        None,
        &mut visited_static_roots,
        |context, value| {
            let array = value.as_array();
            if array.len() == 1 {
                let id = address_to_index(&heap, value.get_object_address());
                context.push(id);
            }
        },
    );
    assert_eq!(visited_static_roots, root_indices);
}

unsafe fn check_visit_continuation_table(heap: &MotokoHeap, continuation_indices: &[ObjectIdx]) {
    let roots = get_roots(heap);
    let mut visited_continuations = vec![];
    visit_roots(
        roots,
        heap.heap_base_address(),
        None,
        &mut visited_continuations,
        |context, value| {
            let array = value.as_array();
            if array.len() != 1 {
                assert_eq!(context.len(), 0);
                for index in 0..array.len() {
                    let element = array.get(index);
                    let id = address_to_index(&heap, element.get_object_address());
                    context.push(id);
                }
            }
        },
    );
    assert_eq!(visited_continuations, continuation_indices);
}

unsafe fn check_visit_remembered_set() {
    let object_map: [(ObjectIdx, Vec<ObjectIdx>); 16] = from_fn(|id| (id as u32, vec![]));
    let root_indices = [];
    let continuation_indices = [3, 5, 11, 13];
    let remembered_set_indices = [2, 7, 9, 14];

    let heap = MotokoHeap::new(
        &object_map,
        &root_indices,
        &continuation_indices,
        GC::Incremental,
    );
    let mut remembered_set_values =
        remembered_set_indices.map(|index| index_to_object_id(&heap, index as usize));
    let mut mem = TestMemory::new(Words(WORD_SIZE as u32 * INITIAL_TABLE_LENGTH));

    let mut remembered_set = RememberedSet::new(&mut mem);
    for index in 0..remembered_set_values.len() {
        let location = &mut remembered_set_values[index] as *mut Value;
        remembered_set.insert(&mut mem, Value::from_raw(location as u32));
    }

    let roots = get_roots(&heap);
    let mut visited_remembered_values = vec![];
    visit_roots(
        roots,
        heap.heap_base_address(),
        Some(&remembered_set),
        &mut visited_remembered_values,
        |context, value| {
            let array = value.as_array();
            if array.len() == 1 {
                let id = address_to_index(&heap, value.get_object_address());
                context.push(id);
            }
        },
    );
    assert_eq!(visited_remembered_values, remembered_set_indices);

    OBJECT_TABLE = None;
}

unsafe fn get_roots(heap: &MotokoHeap) -> Roots {
    let static_roots = heap.static_root_array_id();
    let continuation_table_location = heap.continuation_table_ptr_address() as *mut Value;
    assert_ne!(continuation_table_location, null_mut());
    Roots {
        static_roots,
        continuation_table_location,
    }
}

fn address_to_index(heap: &MotokoHeap, address: usize) -> u32 {
    let offset = address - heap.heap_base_address();
    const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;
    assert_eq!(OBJECT_SIZE, 16);
    assert_eq!(offset % OBJECT_SIZE, 0);
    (offset / OBJECT_SIZE) as u32
}

fn index_to_object_id(heap: &MotokoHeap, index: usize) -> Value {
    const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;
    assert_eq!(OBJECT_SIZE, 16);
    let address = heap.heap_base_address() + index * OBJECT_SIZE;
    unsafe { (address as *mut Obj).object_id() }
}
