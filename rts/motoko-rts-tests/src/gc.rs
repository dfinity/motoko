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

use motoko_rts::gc::copying::copying_gc_internal;
use motoko_rts::gc::mark_compact::compacting_gc_internal;
use motoko_rts::types::*;

use std::collections::{HashMap, HashSet};
use std::fmt::Write;

pub fn test() {
    println!("Testing garbage collection ...");

    // TODO: Add more tests

    let heap = hashmap! {
        0 => vec![0, 2],
        2 => vec![0],
        3 => vec![3],
    };

    let roots = vec![0, 2, 3];

    test_gcs(&TestHeap { heap, roots });
}

#[derive(Debug)]
struct TestHeap {
    heap: HashMap<ObjectIdx, Vec<ObjectIdx>>,
    roots: Vec<ObjectIdx>,
}

/// Test all GC implementations with the given heap
fn test_gcs(heap_descr: &TestHeap) {
    for gc in &GC_IMPLS {
        test_gc(*gc, &heap_descr.heap, &heap_descr.roots);
    }
}

fn test_gc(gc: GC, refs: &HashMap<u32, Vec<u32>>, roots: &[u32]) {
    let heap = MotokoHeap::new(refs, roots, gc);

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
/// - All and only reachable objects should be in the heap. Reachable objects are those in the
///   transitive closure of roots.
///
/// - Objects should point to right objects. E.g. if object with index X points to objects with
///   indices Y and Z in the `objects` map, it should point to objects with indicex Y and Z on the
///   heap.
///
fn check_dynamic_heap(
    objects: &HashMap<ObjectIdx, Vec<ObjectIdx>>,
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

        // There should be at least one field for the index
        assert!(n_fields >= 1);

        let object_idx = read_word(heap, offset) >> 1;
        offset += WORD_SIZE;
        let old = seen.insert(object_idx, address);
        if let Some(old) = old {
            panic!(
                "Object with index {} seen multiple times: {:#x}, {:#x}",
                object_idx, old, address
            );
        }

        let object_expected_pointees = objects.get(&object_idx).unwrap_or_else(|| {
            panic!("Object with index {} is not in the objects map", object_idx)
        });

        for field_idx in 1..n_fields {
            let field = read_word(heap, offset);
            offset += WORD_SIZE;
            // Get index of the object pointed by the field
            let pointee_address = field.wrapping_add(1); // unskew
            let pointee_offset = (pointee_address as usize) - (heap.as_ptr() as usize);
            let pointee_idx_offset = pointee_offset as usize + 2 * WORD_SIZE; // skip header + length
            let pointee_idx = read_word(heap, pointee_idx_offset) >> 1;
            let expected_pointee_idx = object_expected_pointees[(field_idx - 1) as usize];
            assert_eq!(
                pointee_idx,
                expected_pointee_idx,
                "Object with index {} points to {} in field {}, but expected to point to {}",
                object_idx,
                pointee_idx,
                field_idx - 1,
                expected_pointee_idx,
            );
        }
    }

    // At this point we've checked that all seen objects point to the expected objects (as
    // specified by `objects`). Check that we've seen the reachable objects and only the reachable
    // objects.
    let reachable_objects = compute_reachable_objects(roots, objects);

    // Objects we've seen in the heap
    let seen_objects: HashSet<ObjectIdx> = seen.keys().copied().collect();

    // Reachable objects that we haven't seen in the heap
    let missing_objects: Vec<ObjectIdx> = reachable_objects
        .difference(&seen_objects)
        .copied()
        .collect();

    // Unreachable objects that we've seen in the heap
    let extra_objects: Vec<ObjectIdx> = seen_objects
        .difference(&reachable_objects)
        .copied()
        .collect();

    let mut error_message = String::new();

    if !missing_objects.is_empty() {
        write!(
            &mut error_message,
            "Reachable objects missing in the post-GC heap: {:?}",
            missing_objects,
        )
        .unwrap();
    }

    if !extra_objects.is_empty() {
        if !error_message.is_empty() {
            error_message.push('\n');
        }

        write!(
            &mut error_message,
            "Unreachable objects seen in the post-GC heap: {:?}",
            extra_objects,
        )
        .unwrap();
    }

    if !error_message.is_empty() {
        panic!("{}", error_message);
    }
}

fn compute_reachable_objects(
    roots: &[ObjectIdx],
    heap: &HashMap<ObjectIdx, Vec<ObjectIdx>>,
) -> HashSet<ObjectIdx> {
    let mut closure: HashSet<ObjectIdx> = roots.iter().copied().collect();
    let mut work_list: Vec<ObjectIdx> = roots.iter().copied().collect();

    while let Some(next) = work_list.pop() {
        let pointees = heap
            .get(&next)
            .unwrap_or_else(|| panic!("Object {} is in the work list, but not in heap", next));

        for pointee in pointees {
            if closure.insert(*pointee) {
                work_list.push(*pointee);
            }
        }
    }

    closure
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
