// Naming conventions:
//
// - offset = index in the "heap" array/slice/vector
// - address = address in the process's address space
//
// To convert an offset into an address, add heap array's address to the offset.

mod heap;
mod utils;

use heap::MotokoHeap;
use utils::{read_word, ObjectIdx, GC, GC_IMPLS, WORD_SIZE};

// use motoko_rts::debug;
use motoko_rts::gc::copying::copying_gc_internal;
use motoko_rts::gc::mark_compact::compacting_gc_internal;
use motoko_rts::types::*;

use std::collections::{BTreeMap, HashMap, HashSet};

pub fn test() {
    println!("Testing garbage collection ...");

    // TODO: Add more tests

    let heap = btreemap! {
        0 => vec![0, 2],
        2 => vec![0],
        3 => vec![3],
    };

    let roots = vec![0, 2, 3];

    test_gcs(&TestHeap { heap, roots });
}

#[derive(Debug)]
struct TestHeap {
    heap: BTreeMap<ObjectIdx, Vec<ObjectIdx>>,
    roots: Vec<ObjectIdx>,
}

/// Test all GC implementations with the given heap
fn test_gcs(heap_descr: &TestHeap) {
    for gc in &GC_IMPLS {
        test_gc(*gc, &heap_descr.heap, &heap_descr.roots);
    }
}

fn test_gc(gc: GC, refs: &BTreeMap<u32, Vec<u32>>, roots: &[u32]) {
    let heap = MotokoHeap::new(refs, roots, gc);

    // println!("{:?}", heap.heap);

    // unsafe {
    //     debug::dump_heap(
    //         heap.heap_base_address() as u32,
    //         heap.heap_ptr_address() as u32,
    //         skew(heap.static_root_array_address()),
    //         heap.closure_table_address() as *mut SkewedPtr,
    //     );
    // }

    // Check `check_dynamic_heap` sanity
    check_dynamic_heap(
        refs,
        &roots,
        &**heap.heap(),
        heap.heap_base_offset(),
        heap.heap_ptr_offset(),
    );

    for _ in 0..3 {
        gc.run(heap.clone());

        let heap_base_offset = heap.heap_base_offset();
        let heap_ptr_offset = heap.heap_ptr_offset();
        check_dynamic_heap(
            refs,
            &roots,
            &**heap.heap(),
            heap_base_offset,
            heap_ptr_offset,
        );
    }
}

/// Check the dynamic heap:
///
/// - Objects should point to objects with the expected tags (as specified by the `objects` argument)
/// - Each tag should be seen at most once
/// - All of `roots` should be seen
///
/// If any of these conditions do not hold this function panics.
fn check_dynamic_heap(
    objects: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>,
    roots: &[ObjectIdx],
    heap: &[u8],
    heap_base_offset: usize,
    heap_ptr_offset: usize,
) {
    // Current offset in the heap
    let mut offset = heap_base_offset;

    // Maps objects to their addresses (not offsets!). Used when debugging duplicate objects.
    let mut seen: HashMap<ObjectIdx, usize> = Default::default();

    while offset < heap_ptr_offset {
        // Address of the current object. Used for debugging.
        let address = offset as usize + heap.as_ptr() as usize;

        let tag = read_word(heap, offset);
        offset += WORD_SIZE;

        if tag == 0 {
            // Found closure table
            continue;
        }

        assert_eq!(tag, TAG_ARRAY);

        let n_fields = read_word(heap, offset);
        offset += WORD_SIZE;

        // There should be at least one field for the tag
        assert!(n_fields >= 1);

        let tag = read_word(heap, offset) >> 1;
        offset += WORD_SIZE;
        let old = seen.insert(tag, address);
        if let Some(old) = old {
            panic!(
                "Object with tag {} seen multiple times: {:#x}, {:#x}",
                tag, old, address
            );
        }

        let object_expected_pointees = objects
            .get(&tag)
            .unwrap_or_else(|| panic!("Object with tag {} is not in the objects map", tag));

        for field_idx in 1..n_fields {
            let field = read_word(heap, offset);
            offset += WORD_SIZE;
            // Get tag of the object pointed by the field
            let pointee_address = field.wrapping_add(1); // unskew
            let pointee_offset = (pointee_address as usize) - (heap.as_ptr() as usize);
            let pointee_tag_offset = pointee_offset as usize + 2 * WORD_SIZE; // skip header + length
            let pointee_tag = read_word(heap, pointee_tag_offset) >> 1;
            assert_eq!(
                pointee_tag,
                object_expected_pointees[(field_idx - 1) as usize]
            );
        }
    }

    // Check that all roots are seen
    let root_set: HashSet<ObjectIdx> = roots.iter().copied().collect();
    assert_eq!(seen.keys().copied().collect::<HashSet<_>>(), root_set);
}

impl GC {
    fn run(&self, mut heap: MotokoHeap) {
        let heap_base = heap.heap_base_address() as u32;
        let static_roots = skew(heap.static_root_array_address());
        let closure_table_address = heap.closure_table_address() as *mut SkewedPtr;

        let heap_1 = heap.clone();
        let heap_2 = heap.clone();

        match self {
            GC::Copying => {
                unsafe {
                    copying_gc_internal(
                        &mut heap,
                        heap_base,
                        // get_hp
                        || heap_1.heap_ptr_address(),
                        // set_hp
                        move |hp| heap_2.set_heap_ptr_address(hp as usize),
                        static_roots,
                        closure_table_address,
                        // note_live_size
                        |_live_size| {},
                        // note_reclaimed
                        |_reclaimed| {},
                    );
                }
            }

            GC::MarkCompact => {
                unsafe {
                    compacting_gc_internal(
                        &mut heap,
                        heap_base,
                        // get_hp
                        || heap_1.heap_ptr_address(),
                        // set_hp
                        move |hp| heap_2.set_heap_ptr_address(hp as usize),
                        static_roots,
                        closure_table_address,
                        // note_live_size
                        |_live_size| {},
                        // note_reclaimed
                        |_reclaimed| {},
                    );
                }
            }
        }
    }
}
