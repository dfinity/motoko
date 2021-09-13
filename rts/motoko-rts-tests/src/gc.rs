// Naming conventions:
//
// - offset = index in the "heap" array/slice/vector
// - address = address in the process's address space
//
// To convert an offset into an address, add heap array's address to the offset.

mod heap;
mod utils;

use heap::MotokoHeap;
use utils::{
    get_scalar_value, read_byte, read_word, unskew_pointer, ObjectIdx, GC, GC_IMPLS, WORD_SIZE,
};

use motoko_rts::gc::copying::copying_gc_internal;
use motoko_rts::gc::mark_compact::compacting_gc_internal;
use motoko_rts::types::*;

use std::fmt::Write;

use fxhash::{FxHashMap, FxHashSet};

pub fn test() {
    println!("Testing garbage collection ...");

    // TODO: Add more tests

    for test_heap in test_heaps() {
        test_gcs(&test_heap);
    }
}

fn test_heaps() -> Vec<TestHeap> {
    vec![
        // Just a random test that covers a bunch of cases:
        // - Self references
        // - Unreachable objects
        // - Forward pointers
        // - Backwards pointers
        // - More than one fields in an object
        TestHeap {
            heap: vec![
                (0, vec![0, 2]),
                (1, vec![0, 1, 2, 3]),
                (2, vec![0]),
                (3, vec![3]),
            ],
            roots: vec![0, 2, 3],
            continuation_table: vec![0],
        },
        // Tests pointing to the same object in multiple fields of an object. Also has unreachable
        // objects.
        TestHeap {
            heap: vec![(0, vec![]), (1, vec![]), (2, vec![])],
            roots: vec![1],
            continuation_table: vec![0, 0],
        },
        // Root points backwards in heap. Caught a bug in mark-compact collector.
        TestHeap {
            heap: vec![(0, vec![]), (1, vec![2]), (2, vec![1])],
            roots: vec![2],
            continuation_table: vec![],
        },
    ]
}

// All fields are vectors to preserve ordering. Objects are allocated/ added to root arrays etc. in
// the same order they appear in these vectors. Each object in `heap` should have a unique index,
// which is checked when creating the heap.
#[derive(Debug)]
struct TestHeap {
    heap: Vec<(ObjectIdx, Vec<ObjectIdx>)>,
    roots: Vec<ObjectIdx>,
    continuation_table: Vec<ObjectIdx>,
}

/// Test all GC implementations with the given heap
fn test_gcs(heap_descr: &TestHeap) {
    for gc in &GC_IMPLS {
        test_gc(
            *gc,
            &heap_descr.heap,
            &heap_descr.roots,
            &heap_descr.continuation_table,
        );
    }
}

fn test_gc(
    gc: GC,
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
) {
    let heap = MotokoHeap::new(refs, roots, continuation_table, gc);

    // Check `create_dynamic_heap` sanity
    check_dynamic_heap(
        false, // before gc
        refs,
        roots,
        continuation_table,
        &**heap.heap(),
        heap.heap_base_offset(),
        heap.heap_ptr_offset(),
        heap.continuation_table_ptr_offset(),
    );

    for _ in 0..3 {
        gc.run(heap.clone());

        let heap_base_offset = heap.heap_base_offset();
        let heap_ptr_offset = heap.heap_ptr_offset();
        let continuation_table_ptr_offset = heap.continuation_table_ptr_offset();
        check_dynamic_heap(
            true, // after gc
            refs,
            roots,
            continuation_table,
            &**heap.heap(),
            heap_base_offset,
            heap_ptr_offset,
            continuation_table_ptr_offset,
        );
    }
}

/// Check the dynamic heap:
///
/// - All (and in post-gc mode, only) reachable objects should be in the heap. Reachable objects
///   are those in the transitive closure of roots.
///
/// - Objects should point to right objects. E.g. if object with index X points to objects with
///   indices Y and Z in the `objects` map, it should point to objects with indices Y and Z on the
///   heap.
///
fn check_dynamic_heap(
    post_gc: bool,
    objects: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
    heap: &[u8],
    heap_base_offset: usize,
    heap_ptr_offset: usize,
    continuation_table_ptr_offset: usize,
) {
    let objects_map: FxHashMap<ObjectIdx, &[ObjectIdx]> = objects
        .iter()
        .map(|(obj, refs)| (*obj, refs.as_slice()))
        .collect();

    // Current offset in the heap
    let mut offset = heap_base_offset;

    // Maps objects to their addresses (not offsets!). Used when debugging duplicate objects.
    let mut seen: FxHashMap<ObjectIdx, usize> = Default::default();

    let continuation_table_addr = unskew_pointer(read_word(heap, continuation_table_ptr_offset));
    let continuation_table_offset = continuation_table_addr as usize - heap.as_ptr() as usize;

    while offset < heap_ptr_offset {
        let object_offset = offset;

        // Address of the current object. Used for debugging.
        let address = offset as usize + heap.as_ptr() as usize;

        if object_offset == continuation_table_offset {
            check_continuation_table(object_offset, continuation_table, heap);
            offset += (size_of::<Array>() + Words(continuation_table.len() as u32))
                .to_bytes()
                .0 as usize;
            continue;
        }

        let tag = read_byte(heap, offset);
        // Skip tag and rest of the header
        offset += WORD_SIZE;

        assert_eq!(tag, TAG_ARRAY);

        let n_fields = read_word(heap, offset);
        offset += WORD_SIZE;

        // There should be at least one field for the index
        assert!(n_fields >= 1);

        let object_idx = get_scalar_value(read_word(heap, offset));
        offset += WORD_SIZE;
        let old = seen.insert(object_idx, address);
        if let Some(old) = old {
            panic!(
                "Object with index {} seen multiple times: {:#x}, {:#x}",
                object_idx, old, address
            );
        }

        let object_expected_pointees = objects_map.get(&object_idx).unwrap_or_else(|| {
            panic!("Object with index {} is not in the objects map", object_idx)
        });

        for field_idx in 1..n_fields {
            let field = read_word(heap, offset);
            offset += WORD_SIZE;
            // Get index of the object pointed by the field
            let pointee_address = field.wrapping_add(1); // unskew
            let pointee_offset = (pointee_address as usize) - (heap.as_ptr() as usize);
            let pointee_idx_offset = pointee_offset as usize + 2 * WORD_SIZE; // skip header + length
            let pointee_idx = get_scalar_value(read_word(heap, pointee_idx_offset));
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
    let reachable_objects = compute_reachable_objects(roots, continuation_table, &objects_map);

    // Objects we've seen in the heap
    let seen_objects: FxHashSet<ObjectIdx> = seen.keys().copied().collect();

    // Reachable objects that we haven't seen in the heap
    let missing_objects: Vec<ObjectIdx> = reachable_objects
        .difference(&seen_objects)
        .copied()
        .collect();

    let mut error_message = String::new();

    if !missing_objects.is_empty() {
        write!(
            &mut error_message,
            "Reachable objects missing in the {} heap: {:?}",
            if post_gc { "post-gc" } else { "pre-gc" },
            missing_objects,
        )
        .unwrap();
    }

    if post_gc {
        // Unreachable objects that we've seen in the heap
        let extra_objects: Vec<ObjectIdx> = seen_objects
            .difference(&reachable_objects)
            .copied()
            .collect();

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
    }

    if !error_message.is_empty() {
        panic!("{}", error_message);
    }
}

fn compute_reachable_objects(
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
    heap: &FxHashMap<ObjectIdx, &[ObjectIdx]>,
) -> FxHashSet<ObjectIdx> {
    let root_iter = roots.iter().chain(continuation_table.iter()).copied();

    let mut closure: FxHashSet<ObjectIdx> = root_iter.clone().collect();
    let mut work_list: Vec<ObjectIdx> = root_iter.collect();

    while let Some(next) = work_list.pop() {
        let pointees = *heap
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

fn check_continuation_table(mut offset: usize, continuation_table: &[ObjectIdx], heap: &[u8]) {
    assert_eq!(read_byte(heap, offset), TAG_ARRAY);
    // Skip tag and rest of the header
    offset += WORD_SIZE;

    assert_eq!(read_word(heap, offset), continuation_table.len() as u32);
    offset += WORD_SIZE;

    for obj in continuation_table.iter() {
        let ptr = unskew_pointer(read_word(heap, offset));
        offset += WORD_SIZE;

        // Skip object header for idx
        let idx_address = ptr as usize + size_of::<Array>().to_bytes().0 as usize;
        let idx = get_scalar_value(read_word(heap, idx_address - heap.as_ptr() as usize));

        assert_eq!(idx, *obj);
    }
}

impl GC {
    fn run(&self, mut heap: MotokoHeap) {
        let heap_base = heap.heap_base_address() as u32;
        let static_roots = Value::from_ptr(heap.static_root_array_address());
        let continuation_table_ptr_address = heap.continuation_table_ptr_address() as *mut Value;

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
                        continuation_table_ptr_address,
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
                        continuation_table_ptr_address,
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
