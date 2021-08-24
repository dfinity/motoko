use super::utils::{
    make_pointer, make_scalar, write_word, ObjectIdx, GC, MAX_MARK_STACK_SIZE, WORD_SIZE,
};

use motoko_rts::page_alloc::PageAlloc;
use motoko_rts::space::Space;
use motoko_rts::types::*;

use std::cell::{Ref, RefCell};
use std::convert::TryFrom;
use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};

pub unsafe fn create_motoko_heap<P: PageAlloc>(
    page_alloc: &mut P,
    map: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
) -> Space<P> {
    // Check test correctness: an object should appear at most once in `map`
    {
        let heap_objects: FxHashSet<ObjectIdx> = map.iter().map(|(obj, _)| *obj).collect();
        assert_eq!(
            heap_objects.len(),
            map.len(),
            "Invalid test heap: some objects appear multiple times"
        );
    }

    let mut space = Space::new(page_alloc.clone());

    let (mutbox_ptrs, closure_tbl_ptr_ptr) =
        create_static_heap(&mut space, u32::try_from(roots.len()).unwrap());

    let (obj_addrs, closure_tbl_ptr) = create_dynamic_heap(&mut space, map, continuation_table);

    // Update root MutBox fields
    for (root_idx, root_mutbox) in roots.iter().zip(mutbox_ptrs.iter()) {
        let mutbox = root_mutbox.unskew() as *mut MutBox;
        let root_ptr = *obj_addrs.get(root_idx).unwrap();
        (*mutbox).field = root_ptr;
    }

    // Update closure table ptr location
    *(closure_tbl_ptr_ptr.as_ptr() as *mut Value) = closure_tbl_ptr;

    space
}

/// Creates static part of the heap, with space left for continuation table pointer and the roots.
/// Returns:
///
/// 1. Pointers to MutBoxes for the roots. Nth root will need to be pointed by Nth MutBox in the
///    vector.
///
/// 2. Pointer to the closure table pointer.
unsafe fn create_static_heap<P: PageAlloc>(
    space: &mut Space<P>,
    n_roots: u32,
) -> (Vec<Value>, Value) {
    // The layout is:
    //
    // - Array of MutBoxes for the roots (root array). This part does not need to be updated later
    //   after allocating dynamic objects.
    //
    // - MutBoxes. Fields of these MutBoxes need to be updated with pointers to dynamic hepa.
    //
    // - Continuation table pointer. This location needs to be updated with the location of
    //   continuation table in dynamic heap.

    // Allocate the root array
    let root_array_size = Words(n_roots) + size_of::<Array>();
    let root_array_ptr = space.alloc_words(root_array_size);
    let root_array = root_array_ptr.unskew() as *mut Array;
    (*root_array).header.tag = TAG_ARRAY;
    (*root_array).len = n_roots;

    // Allocate MutBoxes for roots, add MutBoxes to the root array
    let mut mutbox_ptrs = Vec::with_capacity(n_roots as usize);
    for i in 0..n_roots {
        let mutbox = space.alloc_words(size_of::<MutBox>());
        root_array.set(i, mutbox);
        // Field unset at this point, will be updated after allocating dynamic heap

        mutbox_ptrs.push(mutbox);
    }

    let closure_table_pointer_loc = space.alloc_words(Words(1));

    (mutbox_ptrs, closure_table_pointer_loc)
}

/// Creates dynamic part of the heap. Returns:
///
/// 1. A map from objects indices (tags) to pointers to the objects. Used to update root fields in
///    static heap
///
/// 2. Pointer to the continuation table
unsafe fn create_dynamic_heap<P: PageAlloc>(
    space: &mut Space<P>,
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    continuation_table: &[ObjectIdx],
) -> (FxHashMap<ObjectIdx, Value>, Value) {
    // First pass allocates objects and collects object addresses. Second pass fills the fields
    // with addresses collected in the first pass.

    // Maps objects to their addresses
    let mut object_ptrs: FxHashMap<ObjectIdx, Value> = Default::default();

    // Allocate objects
    for (obj_idx, refs) in refs {
        // +2 for header + tag (index)
        let obj_size = Words(refs.len() as u32) + Words(2);
        let obj_ptr = space.alloc_words(obj_size);

        object_ptrs.insert(*obj_idx, obj_ptr);

        let obj = obj_ptr.unskew() as *mut Array;

        (*obj).header.tag = TAG_ARRAY;
        (*obj).len = refs.len() as u32 + 1; // +1 for tag (index)
        obj.set(0, Value::from_scalar(*obj_idx)); // tag (index)

        // Pointer fields will be set in the second pass
    }

    // Add fields
    for (obj_idx, refs) in refs {
        let obj_ptr = object_ptrs.get(obj_idx).unwrap();
        let obj = obj_ptr.unskew() as *mut Array;

        for (ref_idx, ref_) in refs.iter().enumerate() {
            let ref_ptr = object_ptrs.get(ref_).unwrap();
            obj.set(ref_idx as u32 + 1, *ref_ptr);
        }
    }

    // Allocate continuation table
    let cont_tbl_size = continuation_table.len() + 2;
    let cont_tbl_ptr = space.alloc_words(Words(cont_tbl_size as u32));
    let cont_tbl = cont_tbl_ptr.unskew() as *mut Array;
    (*cont_tbl).header.tag = TAG_ARRAY;
    (*cont_tbl).len = continuation_table.len() as u32;

    for (i, obj_idx) in continuation_table.iter().enumerate() {
        let obj_ptr = object_ptrs.get(obj_idx).unwrap();
        cont_tbl.set(i as u32, *obj_ptr);
    }

    (object_ptrs, cont_tbl_ptr)
}
