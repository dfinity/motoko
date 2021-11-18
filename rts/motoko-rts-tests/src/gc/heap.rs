use super::utils::ObjectIdx;

use motoko_rts::constants::WORD_SIZE;
use motoko_rts::page_alloc::PageAlloc;
use motoko_rts::space::Space;
use motoko_rts::types::*;

use std::alloc::Layout;
use std::convert::TryFrom;

use fxhash::{FxHashMap, FxHashSet};

pub struct MotokoHeap<P: PageAlloc> {
    pub static_heap: StaticHeap,
    pub space: Space<P>,
}

pub struct StaticHeap {
    pub mem: *mut u8,
    pub layout: Layout,
    pub mutbox_ptrs: Vec<*mut MutBox>,

    /// Pointer to the continuation table pointer
    pub cont_tbl_loc: *mut Value,

    /// Pointer to the root array
    pub root_array: *mut Array,
}

pub unsafe fn create_motoko_heap<P: PageAlloc>(
    page_alloc: &mut P,
    map: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    cont_tbl: &[ObjectIdx],
) -> MotokoHeap<P> {
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

    let static_heap = create_static_heap(&mut space, u32::try_from(roots.len()).unwrap());

    let (obj_addrs, cont_tbl) = create_dynamic_heap(&mut space, map, cont_tbl);

    // Update root MutBox fields
    for (root_idx, mutbox) in roots.iter().zip(&static_heap.mutbox_ptrs) {
        let root_ptr = *obj_addrs.get(root_idx).unwrap();
        mutbox.set_field(root_ptr);
    }

    // Update continuation table ptr location
    *static_heap.cont_tbl_loc = Value::from_ptr(cont_tbl as usize);

    MotokoHeap { static_heap, space }
}

/// Creates static part of the heap, with space left for continuation table pointer and the roots.
/// Returns:
///
/// 1. Pointers to MutBoxes for the roots. Nth root will need to be pointed by Nth MutBox in the
///    vector.
///
/// 2. Pointer to the continuation table pointer.
///
/// Use the heap pointer of `space` to get the static heap size after calling this function.
unsafe fn create_static_heap<P: PageAlloc>(space: &mut Space<P>, n_roots: u32) -> StaticHeap {
    // The layout is:
    //
    // - Array of MutBoxes for the roots (root array). This part does not need to be updated later
    //   after allocating dynamic objects.
    //
    // - MutBoxes. Fields of these MutBoxes need to be updated with pointers to dynamic hepa.
    //
    // - Continuation table pointer. This location needs to be updated with the location of
    //   continuation table in dynamic heap.

    let root_array_size = Words(n_roots) + size_of::<Array>();
    let mutbox_size = size_of::<MutBox>() * n_roots;
    let cont_tbl_loc_size = Words(1);

    let total_size = root_array_size + mutbox_size + cont_tbl_loc_size;

    let static_heap_layout =
        Layout::from_size_align(total_size.to_bytes().as_usize(), WORD_SIZE as usize).unwrap();

    let static_heap: *mut u8 = std::alloc::alloc(static_heap_layout);
    assert!(!static_heap.is_null());

    libc::memset(static_heap as *mut _, 0, total_size.to_bytes().as_usize());

    // Allocate the root array
    let root_array = static_heap as *mut Array;
    root_array.set_tag();
    root_array.set_static();
    root_array.set_len(n_roots);

    // Allocate MutBoxes for roots, add MutBoxes to the root array
    let mut mutbox_ptrs = Vec::with_capacity(n_roots as usize);

    let mut mutbox_addr: usize = static_heap.add(root_array_size.to_bytes().as_usize()) as usize;
    for i in 0..n_roots {
        let mutbox = mutbox_addr as *mut MutBox;
        mutbox.set_tag();
        mutbox.set_static();

        // Field unset at this point, will be updated after allocating dynamic heap
        root_array.set(i, Value::from_ptr(mutbox_addr));
        mutbox_ptrs.push(mutbox);

        mutbox_addr += size_of::<MutBox>().to_bytes().as_usize();
    }

    let cont_tbl_loc = mutbox_addr as *mut Value;

    StaticHeap {
        mem: static_heap,
        layout: static_heap_layout,
        mutbox_ptrs,
        cont_tbl_loc,
        root_array,
    }
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
) -> (FxHashMap<ObjectIdx, Value>, *mut Array) {
    // First pass allocates objects and collects object addresses. Second pass fills the fields
    // with addresses collected in the first pass.

    // Maps objects to their addresses
    let mut object_ptrs: FxHashMap<ObjectIdx, Value> = Default::default();

    // Allocate objects
    for (obj_idx, refs) in refs {
        // +2 for header + tag (index)
        let obj_size = Words(refs.len() as u32) + Words(3);
        let obj_ptr = space.alloc_words(obj_size);

        object_ptrs.insert(*obj_idx, obj_ptr);

        let array = obj_ptr.get_ptr() as *mut Array;

        array.set_tag();
        array.set_len(refs.len() as u32 + 1); // +1 for tag
        array.set(0, Value::from_scalar(*obj_idx)); // tag

        assert!(array.get(0).is_scalar());

        // Pointer fields will be set in the second pass
    }

    // Remove `mut`
    let object_ptrs = object_ptrs;

    // dbg!(&object_ptrs);
    // dbg!(&object_ptrs as *const _ as usize);

    // Second pass, add fields
    for (obj_idx, refs) in refs {
        // dbg!(&object_ptrs);

        let obj_ptr = object_ptrs.get(obj_idx).unwrap();
        let array = obj_ptr.get_ptr() as *mut Array;

        for (ref_idx, ref_) in refs.iter().enumerate() {
            // dbg!(&object_ptrs);

            let ref_ptr = object_ptrs.get(ref_).unwrap_or_else(|| {
                panic!("Could not find {} in object_ptrs: {:?}", ref_, object_ptrs)
            });

            // dbg!(&object_ptrs);
            // dbg!(ref_idx);
            array.set(ref_idx as u32 + 1, *ref_ptr);
            // dbg!(&object_ptrs);
        }
    }

    // Allocate continuation table
    let cont_tbl_size = continuation_table.len() + 2;
    let cont_tbl_ptr = space.alloc_words(Words(cont_tbl_size as u32));
    let cont_tbl = cont_tbl_ptr.get_ptr() as *mut Array;
    cont_tbl.set_tag();
    cont_tbl.set_len(continuation_table.len() as u32);

    for (i, obj_idx) in continuation_table.iter().enumerate() {
        let obj_ptr = object_ptrs.get(obj_idx).unwrap();
        cont_tbl.set(i as u32, *obj_ptr);
    }

    (object_ptrs, cont_tbl)
}
