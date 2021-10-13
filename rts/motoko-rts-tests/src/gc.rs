// Naming conventions:
//
// - offset = index in the "heap" array/slice/vector
// - address = address in the process's address space
//
// To convert an offset into an address, add heap array's address to the offset.

mod heap;
mod utils;

use crate::page_alloc::TestPageAlloc;
use utils::{ObjectIdx, GC, GC_IMPLS, WORD_SIZE};

use motoko_rts::gc::copying::copying_gc_internal;
use motoko_rts::gc::mark_compact::compacting_gc_internal;
use motoko_rts::page_alloc::{Page, PageAlloc};
use motoko_rts::space::Space;
use motoko_rts::types::*;

use std::fmt::Write;

use fxhash::{FxHashMap, FxHashSet};

pub fn test() {
    println!("Testing garbage collection ...");

    // TODO: Add more tests

    let mut page_alloc = TestPageAlloc::new();

    for test_heap in test_heaps() {
        test_gcs(&mut page_alloc, &test_heap);
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
fn test_gcs<P: PageAlloc>(page_alloc: &mut P, heap_descr: &TestHeap) {
    // TODO: Fix and enable mark-compact gc
    //for gc in &GC_IMPLS {
    test_gc(
        page_alloc.clone(),
        GC::Copying,
        &heap_descr.heap,
        &heap_descr.roots,
        &heap_descr.continuation_table,
    );
    // }
}

fn test_gc<P: PageAlloc>(
    mut page_alloc: P,
    gc: GC,
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
) {
    let heap::MotokoHeap {
        mut space,
        cont_tbl_loc,
        root_array,
    } = unsafe { heap::create_motoko_heap(&mut page_alloc, refs, roots, continuation_table) };

    // Check `create_dynamic_heap` sanity
    check_dynamic_heap(
        &space,
        false, // before gc
        refs,
        roots,
        continuation_table,
        cont_tbl_loc,
    );

    for _ in 0..3 {
        space = gc.run(
            page_alloc.clone(),
            space,
            Value::from_ptr(root_array as usize),
            cont_tbl_loc,
        );

        check_dynamic_heap(
            &space,
            true, // after gc
            refs,
            roots,
            continuation_table,
            cont_tbl_loc,
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
fn check_dynamic_heap<P: PageAlloc>(
    space: &Space<P>,
    post_gc: bool,
    objects: &[(ObjectIdx, Vec<ObjectIdx>)],
    roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
    cont_tbl_loc: *mut Value,
) {
    // Maps objects to their fields
    let object_map: FxHashMap<ObjectIdx, &[ObjectIdx]> = objects
        .iter()
        .map(|(obj, refs)| (*obj, refs.as_slice()))
        .collect();

    // Maps seen objects to their addresses. Used when debugging duplicate objects.
    let mut seen: FxHashMap<ObjectIdx, usize> = Default::default();

    // Current page
    let mut page_idx = space.first_page();

    // Continuation table
    let cont_tbl = unsafe { *cont_tbl_loc }.get_ptr() as *mut Array;
    assert_eq!(unsafe { (*cont_tbl).header.tag }, TAG_ARRAY);
    assert_eq!(unsafe { cont_tbl.len() }, continuation_table.len() as u32);

    // Scan the space, check that objects are not seen multiple times and have the right fields
    while page_idx <= space.last_page() {
        let page = space.get_page(page_idx).unwrap();

        let mut scan = unsafe { page.contents_start() };

        let end = if page_idx == space.current_page_idx() {
            space.hp()
        } else {
            unsafe { page.end() }
        };

        while scan < end {
            if scan == cont_tbl_loc as usize {
                scan += WORD_SIZE;
                continue;
            }

            if scan == cont_tbl as usize {
                // TODO: check continuation table
                scan += unsafe { object_size(scan) }.to_bytes().as_usize();
                continue;
            }

            let obj = scan as *mut Obj;
            if unsafe { obj.is_static() } {
                scan += unsafe { object_size(scan) }.to_bytes().as_usize();
                continue;
            }

            let obj = scan as *mut Array;
            assert_eq!(unsafe { (obj as *mut Obj).tag() }, TAG_ARRAY);

            let tag = unsafe { obj.get(0) }.get_scalar();
            let expected_fields = object_map.get(&tag).unwrap();

            let old = seen.insert(tag, scan);
            if let Some(old) = old {
                panic!(
                    "Object with tag {} is seen twice: {:#x}, {:#x}",
                    tag, old, scan
                );
            }

            // +1 for the tag
            assert_eq!(unsafe { obj.len() }, expected_fields.len() as u32 + 1);

            // Check that the fields are as expected
            for (field_idx, expected_field_tag) in expected_fields.iter().enumerate() {
                // +1 to skip the tag
                let field = unsafe { obj.get(field_idx as u32 + 1) }.get_ptr();
                let field_tag = unsafe { (field as *mut Array).get(0) }.get_scalar();
                assert_eq!(field_tag, *expected_field_tag);
            }

            scan += unsafe { object_size(scan) }.to_bytes().as_usize();
        }

        page_idx = page_idx.next();
    }

    // At this point we've checked that all seen objects point to the expected objects (as
    // specified by `objects`). Check that we've seen the reachable objects and in post-gc only the
    // reachable objects.
    let reachable_objects = compute_reachable_objects(roots, continuation_table, &object_map);

    let seen_objects: FxHashSet<ObjectIdx> = seen.keys().copied().collect();

    let missing_reachable_objects: Vec<ObjectIdx> = reachable_objects
        .difference(&seen_objects)
        .copied()
        .collect();

    let mut error_message = String::new();

    if !missing_reachable_objects.is_empty() {
        write!(
            &mut error_message,
            "Reachable objects missing in the {} heap: {:?}",
            if post_gc { "post-gc" } else { "pre-gc" },
            missing_reachable_objects,
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

fn check_continuation_table(cont_tbl_addr: usize, cont_tbl: &[ObjectIdx]) {
    let cont_tbl_ = cont_tbl_addr as *mut Array;
    assert_eq!(unsafe { (*cont_tbl_).header.tag }, TAG_ARRAY);
    assert_eq!(unsafe { cont_tbl_.len() }, cont_tbl.len() as u32);

    for (i, obj_tag) in cont_tbl.iter().enumerate() {
        let field = unsafe { cont_tbl_.get(i as u32) }.get_ptr();
        let field_tag = unsafe { (field as *mut Array).get(1) }.get_scalar();
        assert_eq!(field_tag, *obj_tag);
    }
}

impl GC {
    fn run<P: PageAlloc>(
        &self,
        page_alloc: P,
        mut space: Space<P>,
        static_root_array: Value,
        continuation_table_loc: *mut Value,
    ) -> Space<P> {
        match self {
            GC::Copying => {
                let mut to_space = unsafe { Space::new(page_alloc.clone()) };
                unsafe {
                    copying_gc_internal(
                        &mut to_space,
                        static_root_array,
                        continuation_table_loc,
                        |_| {}, // note_live_size
                        |_| {}, // note_reclaimed
                    );
                }
                to_space
            }
            GC::MarkCompact => unsafe {
                compacting_gc_internal(
                    &page_alloc,
                    &mut space,
                    static_root_array,
                    continuation_table_loc,
                    |_| {}, // note_live_size
                    |_| {}, // note_reclaimed
                );
                space
            },
        }
    }
}
