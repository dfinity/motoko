use motoko_rts::types::*;

use std::collections::{BTreeMap, HashSet};
use std::convert::TryFrom;

use byteorder::{WriteBytesExt, LE};

type ObjectIdx = u32;

const WORD_SIZE: usize = 4;

const TAG_ARRAY: u32 = 3;

pub struct MotokoHeap {
    pub heap: Vec<u8>,

    /// Where the dynamic heap starts
    pub heap_base: usize,

    /// Where the dynamic heap ends
    pub heap_ptr: usize,

    /// Offset of the static root array: an array of pointers below `heap_base`
    pub static_root_array_offset: usize,

    /// Offset of the closure table. Currently we put a tagged scalar to this location and
    /// effectively skip closure table evacuation.
    pub closure_table_offset: usize,
}

impl MotokoHeap {
    pub fn new(map: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>, roots: &[ObjectIdx]) -> MotokoHeap {
        // Each object will be 3 words per object + one word for each reference. Static heap will
        // have an array (header + length) with one word for each root.
        let static_heap_size = (2 + roots.len()) * WORD_SIZE;
        let dynamic_heap_size = {
            let object_headers_words = map.len() * 3;
            let references_words = map.values().map(|refs| refs.len()).sum::<usize>();
            let closure_table_words = 1;
            (object_headers_words + references_words + closure_table_words) * WORD_SIZE
        };
        let total_heap_size_bytes = static_heap_size + dynamic_heap_size;

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
                .write_u32::<LE>(u32::try_from(root_address).unwrap().wrapping_sub(1))
                .unwrap();
            root_addr_offset += WORD_SIZE;
        }

        // Add closure table at the end of the heap. Currently closure table is just a scalar.
        let closure_table_offset = static_heap_size + dynamic_heap_size - WORD_SIZE;
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

    let heap_start = heap.as_ptr() as usize;

    // Maps objects to their addresses in the heap
    let mut object_addrs: Vec<usize> = vec![usize::MAX; last_obj as usize + 1];

    // First pass allocates objects without fields
    {
        let mut heap_offset = heap_base;
        for (obj, refs) in refs {
            object_addrs[*obj as usize] = heap_start + heap_offset;

            // Store object header
            (&mut heap[heap_offset..])
                .write_u32::<LE>(TAG_ARRAY)
                .unwrap();
            heap_offset += WORD_SIZE;

            // Store length: tag + refs
            (&mut heap[heap_offset..])
                .write_u32::<LE>(u32::try_from(refs.len() + 1).unwrap())
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
        let obj_offset = object_addrs[*obj as usize] - heap_start;
        for (ref_idx, ref_) in refs.iter().enumerate() {
            // -1 for skewing
            let ref_addr = object_addrs[*ref_ as usize].wrapping_sub(1);
            let field_offset = obj_offset + (3 + ref_idx) * WORD_SIZE;
            (&mut heap[field_offset..])
                .write_u32::<LE>(u32::try_from(ref_addr).unwrap())
                .unwrap();
        }
    }

    object_addrs
}
