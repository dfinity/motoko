use std::collections::{BTreeMap, HashSet};

type ObjectIdx = u32;

const WORD_SIZE: usize = 4;

const TAG_ARRAY: u32 = 3;

pub fn allocate_heap(refs: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>) -> Vec<u32> {
    if refs.is_empty() {
        return vec![];
    }

    let last_obj = *refs.last_key_value().as_ref().unwrap().0;

    // Number of references in the heap
    let n_refs: usize = refs.values().map(|refs| refs.len()).sum();

    // Heap size in words
    let heap_size = n_refs + (refs.len() * 2);
    let mut heap: Vec<u32> = vec![0u32; heap_size];

    // Maps objects to their addresses in the heap: Nth element is the word offset of the Nth
    // object
    let mut object_addrs: Vec<usize> = vec![usize::MAX; last_obj as usize + 1];

    // First pass allocates objects without fields
    {
        let mut heap_idx: usize = 0;
        for (obj, refs) in refs {
            object_addrs[*obj as usize] = heap_idx;

            // Store object header
            heap[heap_idx] = TAG_ARRAY;

            // Store object value
            heap[heap_idx + 1] = obj << 1;

            heap_idx += 2 + refs.len();
        }
    }

    println!("object addresses={:#?}", object_addrs);

    // Second pass adds pointer fields
    for (obj, refs) in refs {
        let obj_addr = object_addrs[*obj as usize];
        for (ref_idx, ref_) in refs.iter().enumerate() {
            // -1 for skewing
            let ref_addr = object_addrs[*ref_ as usize].wrapping_sub(1);

            let field_addr = obj_addr + 2 + ref_idx;
            heap[field_addr] = ref_addr as u32;
        }
    }

    heap
}

pub fn check_heap(refs: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>, heap: &[u32], heap_end: usize) {
    // Scan the heap, check that objects in the heap point to the right objects, by comparing tags.
    // E.g. if in `refs` object 0 points to 3 and 5, then the object with tag 0 must point to
    // objects with tags 3 and 5 in its second and second third, respectively. (first fields always
    // have tags)

    // Set of visited objects (in the post-GC heap) to make sure (1) we don't duplicate objects
    // (2) we don't miss any objects in the original heap
    let mut visited: HashSet<ObjectIdx> = Default::default();

    // Current heap address
    let mut heap_addr = 0;

    while heap_addr < heap_end {
        // Object tag must tag TAG_ARRAY
        assert_eq!(heap[heap_addr], TAG_ARRAY);

        let object_tag = heap[heap_addr + 1] >> 1;

        let object_expected_refs = refs.get(&object_tag).unwrap_or_else(|| {
            panic!(
                "Object with tag {} not found in the heap description",
                object_tag
            )
        });

        let not_exists = visited.insert(object_tag);
        // Check (1)
        if !not_exists {
            panic!("Object {} visited twice", object_tag);
        }

        // Skip header and tag
        heap_addr += 2;

        for (ref_idx, ref_) in object_expected_refs.iter().copied().enumerate() {
            let addr = heap[heap_addr].wrapping_add(1) as usize;
            let tag = heap[addr + 1] >> 1;

            // Check tag of pointee
            if tag != ref_ {
                panic!(
                    "Post-GC object {} does not have expected reference in field {}: \
                     expected {}, found {}",
                    object_tag, ref_idx, ref_, tag,
                );
            }

            heap_addr += 1;
        }
    }

    // Check (2)
    for ref_ in refs.keys() {
        if !visited.contains(ref_) {
            panic!(
                "Object {} in the original heap not seen in post-GC heap",
                ref_
            );
        }
    }
}
