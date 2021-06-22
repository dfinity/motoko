// Naming conventions:
//
// - offset = index in the "heap" array/slice/vector
// - address = address in the process's address space
//
// To convert an offset into an address, add heap array's address to the offset.

use motoko_rts::debug;
use motoko_rts::gc::copying::copying_gc_internal;
use motoko_rts::heap::Heap;
use motoko_rts::types::*;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryFrom;

use byteorder::{ReadBytesExt, WriteBytesExt, LE};

type ObjectIdx = u32;

const WORD_SIZE: usize = 4;

// We only allocate arrays in the dynamic heap for now
const TAG_ARRAY: u32 = 3;
// MutBox is used for static root array elements (TODO: We could store pointers to dynamic roots
// directly in the array, instead of via MutBox0
const TAG_MUTBOX: u32 = 6;

struct MotokoHeap {
    /// The heap. This is a boxed slice instead of a vector as growing this wouldn't make sense
    /// (all pointers would have to be updated).
    heap: Box<[u8]>,

    /// Where the dynamic heap starts
    heap_base_offset: usize,

    /// Where the dynamic heap ends, i.e. the heap pointer
    heap_ptr_offset: usize,

    /// Offset of the static root array: an array of pointers below `heap_base`
    static_root_array_offset: usize,

    /// Offset of the closure table. Currently we put a tagged scalar to this location and
    /// effectively skip closure table evacuation.
    closure_table_offset: usize,
}

impl Heap for MotokoHeap {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.heap_ptr_address();
        let new_hp = old_hp + bytes.0 as usize;
        self.heap_ptr_offset = new_hp - self.heap.as_ptr() as usize;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        skew(old_hp)
    }

    unsafe fn grow_memory(&mut self, ptr: usize) {
        let heap_end = self.heap.as_ptr() as usize + self.heap.len();
        if ptr > heap_end {
            // We don't allow growing memory in tests, allocate large enough for the test
            panic!(
                "TestHeap::grow_memory called: heap_end={:#x}, grow_memory argument={:#x}",
                heap_end, ptr
            );
        }
    }
}

fn read_word(heap: &[u8], offset: usize) -> u32 {
    (&heap[offset..]).read_u32::<LE>().unwrap()
}

fn write_word(heap: &mut [u8], offset: usize, word: u32) {
    (&mut heap[offset..]).write_u32::<LE>(word).unwrap()
}

impl MotokoHeap {
    fn address_to_offset(&self, address: usize) -> usize {
        address - self.heap.as_ptr() as usize
    }

    fn offset_to_address(&self, offset: usize) -> usize {
        offset + self.heap.as_ptr() as usize
    }

    /// Get heap base in the process's address space
    fn heap_base_address(&self) -> usize {
        self.offset_to_address(self.heap_base_offset)
    }

    /// Get heap pointer (i.e. where the dynamic heap ends) in the process's address space
    fn heap_ptr_address(&self) -> usize {
        self.offset_to_address(self.heap_ptr_offset)
    }

    fn set_heap_ptr_address(&mut self, address: usize) {
        self.heap_ptr_offset = self.address_to_offset(address);
    }

    /// Get static root array address in the process's address space
    fn static_root_array_address(&self) -> usize {
        self.offset_to_address(self.static_root_array_offset)
    }

    /// Get closure table address in the process's address space
    fn closure_table_address(&self) -> usize {
        self.offset_to_address(self.closure_table_offset)
    }

    fn new(map: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>, roots: &[ObjectIdx]) -> MotokoHeap {
        // Each object will be 3 words per object + one word for each reference. Static heap will
        // have an array (header + length) with one element, one MutBox for each root.
        let static_heap_size = (2 + roots.len() + (roots.len() * 2)) * WORD_SIZE;
        let dynamic_heap_size = {
            let object_headers_words = map.len() * 3;
            let references_words = map.values().map(|refs| refs.len()).sum::<usize>();
            let closure_table_words = 1;
            (object_headers_words + references_words + closure_table_words) * WORD_SIZE
        };
        let total_heap_size_bytes = static_heap_size + dynamic_heap_size;

        // Double the dynamic heap size to allow GC
        let mut heap: Vec<u8> = vec![0; total_heap_size_bytes + dynamic_heap_size];

        // Maps `ObjectIdx`s into their offsets in the heap
        let object_addrs: Vec<usize> = allocate_heap(map, &mut heap, static_heap_size);

        // List of root object addresses
        let root_addresses: Vec<usize> = roots
            .iter()
            .map(|obj| object_addrs[*obj as usize])
            .collect();

        // Create static root array. Each element of the array is a MutBox pointing to the actual
        // root.
        write_word(&mut heap, 0, TAG_ARRAY);
        write_word(&mut heap, WORD_SIZE, u32::try_from(roots.len()).unwrap());

        // Current offset in the heap for the next static roots array element
        let mut root_addr_offset = 2 * WORD_SIZE;

        // Current offset in the heap for the MutBox of the next root
        let mut mutbox_offset = (2 + roots.len()) * WORD_SIZE;

        for root_address in root_addresses {
            // Add a MutBox for the object
            write_word(&mut heap, mutbox_offset, TAG_MUTBOX);
            write_word(
                &mut heap,
                mutbox_offset + WORD_SIZE,
                u32::try_from(root_address).unwrap().wrapping_sub(1),
            );

            let mutbox_addr = heap.as_ptr() as usize + mutbox_offset;
            write_word(
                &mut heap,
                root_addr_offset,
                u32::try_from(mutbox_addr).unwrap().wrapping_sub(1),
            );

            root_addr_offset += WORD_SIZE;
            mutbox_offset += 2 * WORD_SIZE;
        }

        // Add closure table at the end of the heap. Currently closure table is just a scalar.
        let closure_table_offset = static_heap_size + dynamic_heap_size - WORD_SIZE;
        write_word(&mut heap, closure_table_offset, 0);

        MotokoHeap {
            heap: heap.into_boxed_slice(),
            heap_base_offset: static_heap_size,
            heap_ptr_offset: total_heap_size_bytes,
            static_root_array_offset: 0,
            closure_table_offset,
        }
    }
}

fn allocate_heap(
    refs: &BTreeMap<ObjectIdx, Vec<ObjectIdx>>,
    heap: &mut [u8],
    heap_base_offset: usize,
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
        let mut heap_offset = heap_base_offset;
        for (obj, refs) in refs {
            object_addrs[*obj as usize] = heap_start + heap_offset;

            // Store object header
            write_word(heap, heap_offset, TAG_ARRAY);
            heap_offset += WORD_SIZE;

            // Store length: tag + refs
            write_word(heap, heap_offset, u32::try_from(refs.len() + 1).unwrap());
            heap_offset += WORD_SIZE;

            // Store object value (tag)
            write_word(heap, heap_offset, obj << 1);
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
            write_word(heap, field_offset, u32::try_from(ref_addr).unwrap());
        }
    }

    object_addrs
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
        offset += 4;

        if tag == 0 {
            // Found closure table
            continue;
        }

        assert_eq!(tag, TAG_ARRAY);

        let n_fields = read_word(heap, offset);
        offset += 4;

        // There should be at least one field for the tag
        assert!(n_fields >= 1);

        let tag = read_word(heap, offset) >> 1;
        offset += 4;
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
            offset += 4;
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
}

pub fn test() {
    let refs = &btreemap! {
        0 => vec![0, 2],
        2 => vec![0],
        3 => vec![3],
    };

    let roots = vec![0, 2, 3];

    let heap = MotokoHeap::new(&refs, &roots);

    println!("{:?}", heap.heap);

    unsafe {
        debug::dump_heap(
            // get_heap_base
            || heap.heap_base_address() as u32,
            // get_hp
            || heap.heap_ptr_address() as u32,
            // get_static_roots
            || skew(heap.static_root_array_address()),
            // get_closure_table_loc
            || heap.closure_table_address() as *mut SkewedPtr,
        );
    }

    // Check `check_dynamic_heap` sanity
    check_dynamic_heap(
        &refs,
        &roots,
        &*heap.heap,
        heap.heap_base_offset,
        heap.heap_ptr_offset,
    );

    for _ in 0..1 {
        let mut new_hp: u32 = 0;

        let heap_base = heap.heap_base_address() as u32;
        let static_roots = skew(heap.static_root_array_address());
        let closure_table_address = heap.closure_table_address() as *mut SkewedPtr;

        // unsafe {
        //     copying_gc_internal(
        //         &mut heap,
        //         heap_base,
        //         // get_hp
        //         || heap.heap_ptr_address() as u32,
        //         // set_hp
        //         |hp| new_hp = hp,
        //         static_roots,
        //         closure_table_address,
        //         // note_live_size
        //         |_live_size| {},
        //         // note_reclaimed
        //         |_reclaimed| {},
        //     );
        // }

        check_dynamic_heap(
            &refs,
            &roots,
            &*heap.heap,
            heap.heap_base_offset,
            new_hp as usize - heap.heap.as_ptr() as usize,
        );
    }
}
