use std::collections::{BTreeMap, HashSet};
use std::convert::TryFrom;

use byteorder::{WriteBytesExt, LE};

type ObjectIdx = u32;

const WORD_SIZE: usize = 4;

const TAG_ARRAY: u32 = 3;

pub struct MotokoHeap {
    heap: Vec<u8>,

    // Where the dynamic heap starts
    heap_base: usize,

    // Where the dynamic heap ends
    heap_ptr: usize,

    // Offset of the static root array: an array of pointers below `heap_base`
    static_root_array_offset: usize,

    // Offset of the closure table. Currently we put a tagged scalar to this location and
    // effectively skip closure table evacuation.
    closure_table_offset: usize,
}

impl MotokoHeap {
    pub fn new(map: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>, roots: &[ObjectIdx]) -> MotokoHeap {
        // Each object will be 2 words per object + one word for each reference. Static heap will
        // have an array (header + length) with one word for each root.
        let static_heap_size = (2 + roots.len()) * WORD_SIZE;
        let dynamic_heap_size = {
            let object_headers_words = map.len() * 2;
            let references_words = map.values().map(|refs| refs.len()).sum::<usize>();
            (object_headers_words + references_words) * WORD_SIZE
        };
        // Add an extra word for the closure table. Currently closure table is just a scalar. This
        // effectively skips closure table evacuation.
        let total_heap_size_bytes = static_heap_size + dynamic_heap_size + WORD_SIZE;

        // TODO: Maybe make this `Box<[u8]>`?
        let mut heap: Vec<u8> = vec![0; total_heap_size_bytes];

        // Maps `ObjectIdx`s into their offsets in the heap
        let object_addrs: Vec<usize> = allocate_heap(map, &mut heap, static_heap_size);

        // List of root object addresses
        let root_addresses: Vec<usize> = roots
            .iter()
            .map(|obj| object_addrs[*obj as usize])
            .collect();

        // Create static root array
        (&mut heap[0..]).write_u32::<LE>(TAG_ARRAY).unwrap();
        (&mut heap[WORD_SIZE..])
            .write_u32::<LE>(u32::try_from(roots.len()).unwrap())
            .unwrap();

        let mut root_addr_offset = 2 * WORD_SIZE;
        for root_address in root_addresses {
            (&mut heap[root_addr_offset..])
                .write_u32::<LE>(u32::try_from(root_address).unwrap())
                .unwrap();
            root_addr_offset += WORD_SIZE;
        }

        // Add closure table at the end of the heap. Currently closure table is just a scalar.
        let closure_table_offset = dynamic_heap_size - WORD_SIZE;
        (&mut heap[closure_table_offset..])
            .write_u32::<LE>(0)
            .unwrap();

        MotokoHeap {
            heap,
            heap_base: static_heap_size,
            heap_ptr: total_heap_size_bytes,
            static_root_array_offset: 0,
            closure_table_offset,
        }
    }
}

fn allocate_heap(
    refs: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>,
    heap: &mut [u8],
    heap_base: usize,
) -> Vec<usize> {
    if refs.is_empty() {
        return vec![];
    }

    let last_obj = *refs.last_key_value().as_ref().unwrap().0;

    // Maps objects to their addresses in the heap
    let mut object_addrs: Vec<usize> = vec![usize::MAX; last_obj as usize + 1];

    // First pass allocates objects without fields
    {
        let mut heap_offset = heap_base;
        for (obj, refs) in refs {
            object_addrs[*obj as usize] = heap_offset;

            // Store object header
            (&mut heap[heap_offset..])
                .write_u32::<LE>(TAG_ARRAY)
                .unwrap();
            heap_offset += WORD_SIZE;

            // Store object value (tag)
            (&mut heap[heap_offset..])
                .write_u32::<LE>(obj << 1)
                .unwrap();
            heap_offset += WORD_SIZE;

            // Leave space for the fields
            heap_offset += refs.len() * WORD_SIZE;
        }
    }

    println!("object addresses={:#?}", object_addrs);

    // Second pass adds fields
    for (obj, refs) in refs {
        let obj_addr = object_addrs[*obj as usize];
        for (ref_idx, ref_) in refs.iter().enumerate() {
            // -1 for skewing
            let ref_addr = object_addrs[*ref_ as usize].wrapping_sub(1);
            let field_addr = obj_addr + (2 + ref_idx) * WORD_SIZE;
            (&mut heap[field_addr..])
                .write_u32::<LE>(u32::try_from(ref_addr).unwrap())
                .unwrap();
        }
    }

    object_addrs
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
