// Naming conventions:
//
// - offset = index in the "heap" array/slice/vector
// - address = address in the process's address space
//
// To convert an offset into an address, add heap array's address to the offset.

mod heap;
mod incremental;
mod random;
mod utils;

use heap::MotokoHeap;
use utils::{get_scalar_value, make_pointer, read_word, unskew_pointer, ObjectIdx, WORD_SIZE};

use motoko_rts::types::*;

use std::fmt::Write;

use fxhash::{FxHashMap, FxHashSet};

pub fn test() {
    println!("Testing garbage collection ...");

    println!("  Testing pre-defined heaps...");
    for test_heap in test_heaps() {
        test_gcs(&test_heap);
    }

    println!("  Testing random heaps...");
    let max_seed = 100;
    for seed in 0..max_seed {
        print!("\r{}/{}", seed + 1, max_seed);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
        test_random_heap(seed, 180);
    }
    print!("\r");

    test_gc_components();
}

fn test_gc_components() {
    incremental::test();
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

fn test_random_heap(seed: u64, max_objects: u32) {
    let random_heap = random::generate(seed, max_objects);
    test_gcs(&random_heap);
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
    test_gc(
        &heap_descr.heap,
        &heap_descr.roots,
        &heap_descr.continuation_table,
    );
    reset_gc();
}

fn test_gc(
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
) {
    let mut heap = MotokoHeap::new(refs, roots, continuation_table);

    initialize_gc(&mut heap);

    // Check `create_dynamic_heap` sanity
    check_dynamic_heap(
        false, // before gc
        refs,
        roots,
        continuation_table,
        &**heap.heap(),
        heap.heap_base_offset(),
        heap.heap_ptr_offset(),
        heap.static_root_array_variable_offset(),
        heap.continuation_table_variable_offset(),
        heap.region0_pointer_variable_offset(),
    );

    for _ in 0..3 {
        let check_all_reclaimed = run(&mut heap);

        let heap_base_offset = heap.heap_base_offset();
        let heap_ptr_offset = heap.heap_ptr_offset();
        let static_array_variable_offset = heap.static_root_array_variable_offset();
        let continuation_table_variable_offset = heap.continuation_table_variable_offset();
        let region0_ptr_offset = heap.region0_pointer_variable_offset();
        check_dynamic_heap(
            check_all_reclaimed, // check for unreachable objects
            refs,
            roots,
            continuation_table,
            &**heap.heap(),
            heap_base_offset,
            heap_ptr_offset,
            static_array_variable_offset,
            continuation_table_variable_offset,
            region0_ptr_offset,
        );
    }
}

fn initialize_gc(heap: &mut MotokoHeap) {
    use motoko_rts::gc::incremental::{
        get_partitioned_heap, set_incremental_gc_state, IncrementalGC,
    };
    unsafe {
        let state = IncrementalGC::initial_gc_state(heap, heap.heap_base_address());
        set_incremental_gc_state(Some(state));
        let allocation_size = heap.heap_ptr_address() - heap.heap_base_address();

        // Synchronize the partitioned heap with one big combined allocation by starting from the base pointer as the heap pointer.
        let result =
            get_partitioned_heap().allocate(heap, Bytes(allocation_size as u32).to_words());
        // Check that the heap pointer (here equals base pointer) is unchanged, i.e. no partition switch has happened.
        // This is a restriction in the unit test where `MotokoHeap` only supports contiguous bump allocation during initialization.
        assert_eq!(result.get_ptr(), heap.heap_base_address());
    }
}

fn reset_gc() {
    use motoko_rts::gc::incremental::set_incremental_gc_state;
    unsafe {
        set_incremental_gc_state(None);
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
    static_root_array_variable_offset: usize,
    continuation_table_variable_offset: usize,
    region0_ptr_offset: usize,
) {
    let objects_map: FxHashMap<ObjectIdx, &[ObjectIdx]> = objects
        .iter()
        .map(|(obj, refs)| (*obj, refs.as_slice()))
        .collect();

    // Current offset in the heap
    let mut offset = heap_base_offset;

    // Maps objects to their addresses (not offsets!). Used when debugging duplicate objects.
    let mut seen: FxHashMap<ObjectIdx, usize> = Default::default();

    let static_root_array_address =
        unskew_pointer(read_word(heap, static_root_array_variable_offset));
    let static_root_array_offset = static_root_array_address as usize - heap.as_ptr() as usize;

    let continuation_table_address =
        unskew_pointer(read_word(heap, continuation_table_variable_offset));
    let continuation_table_offset = continuation_table_address as usize - heap.as_ptr() as usize;

    let region0_addr = unskew_pointer(read_word(heap, region0_ptr_offset));

    while offset < heap_ptr_offset {
        let object_offset = offset;

        // Address of the current object. Used for debugging.
        let address = offset as usize + heap.as_ptr() as usize;

        if object_offset == static_root_array_offset {
            check_static_root_array(object_offset, roots, heap);
            offset += (size_of::<Array>() + Words(roots.len() as u32))
                .to_bytes()
                .as_usize();
            continue;
        }

        if object_offset == continuation_table_offset {
            check_continuation_table(object_offset, continuation_table, heap);
            offset += (size_of::<Array>() + Words(continuation_table.len() as u32))
                .to_bytes()
                .as_usize();
            continue;
        }

        let tag = read_word(heap, offset);
        offset += WORD_SIZE;

        if tag == TAG_ONE_WORD_FILLER {
        } else if tag == TAG_FREE_SPACE {
            let words = read_word(heap, offset) as usize;
            offset += WORD_SIZE;
            offset += words * WORD_SIZE;
        } else {
            let forward;
            forward = read_word(heap, offset);
            offset += WORD_SIZE;

            let is_forwarded = forward != make_pointer(address as u32);

            if tag == TAG_MUTBOX {
                // MutBoxes of static root array, will be scanned indirectly when checking the static root array.
                offset += WORD_SIZE;
            } else if tag == TAG_BLOB {
                assert!(!is_forwarded);
                // in-heap mark stack blobs
                let length = read_word(heap, offset);
                offset += WORD_SIZE + length as usize;
            } else if tag == TAG_REGION {
                if !is_forwarded {
                    assert_eq!(address, region0_addr as usize);
                }
                offset += (size_of::<Region>() - size_of::<Obj>())
                    .to_bytes()
                    .as_usize();
            } else {
                assert!(tag == TAG_ARRAY || tag >= TAG_ARRAY_SLICE_MIN);

                if is_forwarded {
                    let forward_offset = forward as usize - heap.as_ptr() as usize;
                    let length = read_word(
                        heap,
                        forward_offset + size_of::<Obj>().to_bytes().as_usize(),
                    );

                    // Skip stale object version that has been relocated during incremental GC.
                    offset += length as usize * WORD_SIZE;
                } else {
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

                    let object_expected_pointees =
                        objects_map.get(&object_idx).unwrap_or_else(|| {
                            panic!("Object with index {} is not in the objects map", object_idx)
                        });

                    for field_idx in 1..n_fields {
                        let field = read_word(heap, offset);
                        offset += WORD_SIZE;

                        // Get index of the object pointed by the field
                        let pointee_address = field.wrapping_add(1); // unskew

                        let pointee_idx = read_object_id(pointee_address, heap);
                        let expected_pointee_idx =
                            object_expected_pointees[(field_idx - 1) as usize];
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
            }
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

fn check_static_root_array(mut offset: usize, roots: &[ObjectIdx], heap: &[u8]) {
    let array_address = heap.as_ptr() as usize + offset;
    assert_eq!(read_word(heap, offset), TAG_ARRAY);
    offset += WORD_SIZE;

    assert_eq!(read_word(heap, offset), make_pointer(array_address as u32));
    offset += WORD_SIZE;

    assert_eq!(read_word(heap, offset), roots.len() as u32);
    offset += WORD_SIZE;

    for obj in roots.iter() {
        let mutbox_address = unskew_pointer(read_word(heap, offset));
        offset += WORD_SIZE;

        let object_address = unskew_pointer(read_mutbox_field(mutbox_address, heap));
        let idx = read_object_id(object_address, heap);
        assert_eq!(idx, *obj);
    }
}

fn read_mutbox_field(mutbox_address: u32, heap: &[u8]) -> u32 {
    let mut mutbox_offset = mutbox_address as usize - heap.as_ptr() as usize;

    let mutbox_tag = read_word(heap, mutbox_offset);
    assert_eq!(mutbox_tag, TAG_MUTBOX);
    mutbox_offset += WORD_SIZE;

    assert_eq!(read_word(heap, mutbox_offset), make_pointer(mutbox_address));
    mutbox_offset += WORD_SIZE;

    read_word(heap, mutbox_offset)
}

fn check_continuation_table(mut offset: usize, continuation_table: &[ObjectIdx], heap: &[u8]) {
    let table_addr = heap.as_ptr() as usize + offset;
    assert_eq!(read_word(heap, offset), TAG_ARRAY);
    offset += WORD_SIZE;

    assert_eq!(read_word(heap, offset), make_pointer(table_addr as u32));
    offset += WORD_SIZE;

    assert_eq!(read_word(heap, offset), continuation_table.len() as u32);
    offset += WORD_SIZE;

    for obj in continuation_table.iter() {
        let ptr = unskew_pointer(read_word(heap, offset));
        offset += WORD_SIZE;

        let idx = read_object_id(ptr, heap);
        assert_eq!(idx, *obj);
    }
}

fn read_object_id(object_address: u32, heap: &[u8]) -> ObjectIdx {
    let tag = read_word(heap, object_address as usize - heap.as_ptr() as usize);
    assert!(tag == TAG_ARRAY || tag >= TAG_ARRAY_SLICE_MIN);

    // Skip object header for idx
    let idx_address = object_address as usize + size_of::<Array>().to_bytes().as_usize();
    get_scalar_value(read_word(heap, idx_address - heap.as_ptr() as usize))
}

fn run(heap: &mut MotokoHeap) -> bool {
    let static_root = heap.static_root_array_variable_address() as *mut Value;
    let continuation_table_location = heap.continuation_table_variable_address() as *mut Value;
    let region0_pointer_location = heap.region0_pointer_variable_address() as *mut Value;
    let unused_root = &mut Value::from_scalar(0) as *mut Value;

    unsafe {
        use motoko_rts::gc::incremental::{get_incremental_gc_state, IncrementalGC};
        const INCREMENTS_UNTIL_COMPLETION: usize = 16;
        for _ in 0..INCREMENTS_UNTIL_COMPLETION {
            let roots = [
                static_root,
                continuation_table_location,
                region0_pointer_location,
                unused_root,
                unused_root,
                unused_root,
                unused_root,
            ];
            IncrementalGC::instance(heap, get_incremental_gc_state())
                .empty_call_stack_increment(roots);
        }
        false
    }
}
