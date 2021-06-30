use super::utils::{write_word, ObjectIdx, GC, MAX_MARK_STACK_SIZE, WORD_SIZE};

use motoko_rts::gc::mark_compact::mark_stack::INIT_STACK_SIZE;
use motoko_rts::memory::Memory;
use motoko_rts::types::*;

use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

/// Represents Motoko heaps. Reference counted (implements `Clone`) so we can clone and move values
/// of this type to GC callbacks.
#[derive(Clone)]
pub struct MotokoHeap {
    inner: Rc<RefCell<MotokoHeapInner>>,
}

impl Memory for MotokoHeap {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        self.inner.borrow_mut().alloc_words(n)
    }
}

impl MotokoHeap {
    /// Create a new Motoko heap from the given object graph and roots. `GC` argument is used to
    /// allocate as little space as possible for the dynamic heap.
    ///
    /// Note that for `GC::MarkCompact` we limit the upper bound on mark stack size as
    /// `super::MAX_MARK_STACK_SIZE`. In the worst case the size would be the same as the heap
    /// size, but that's not a realistic scenario.
    pub fn new(
        map: &HashMap<ObjectIdx, Vec<ObjectIdx>>,
        roots: &[ObjectIdx],
        gc: GC,
    ) -> MotokoHeap {
        MotokoHeap {
            inner: Rc::new(RefCell::new(MotokoHeapInner::new(map, roots, gc))),
        }
    }

    /// Get the beginning of dynamic heap, as offset in the heap array
    pub fn heap_base_offset(&self) -> usize {
        self.inner.borrow().heap_base_offset
    }

    /// Get the heap pointer, as offset in the heap array
    pub fn heap_ptr_offset(&self) -> usize {
        self.inner.borrow().heap_ptr_offset
    }

    /// Get the heap pointer, as address in the current process. The address can be used to mutate
    /// the heap.
    pub fn heap_ptr_address(&self) -> usize {
        self.inner.borrow().heap_ptr_address()
    }

    /// Update the heap pointer given as an address in the current process.
    pub fn set_heap_ptr_address(&self, address: usize) {
        self.inner.borrow_mut().set_heap_ptr_address(address)
    }

    /// Get the beginning of dynamic heap, as an address in the current process
    pub fn heap_base_address(&self) -> usize {
        self.inner.borrow().heap_base_address()
    }

    /// Get the address of the static root array
    pub fn static_root_array_address(&self) -> usize {
        self.inner.borrow().static_root_array_address()
    }

    /// Get the address of the closure table
    pub fn closure_table_address(&self) -> usize {
        self.inner.borrow().closure_table_address()
    }

    /// Get the heap as an array. Use `offset` values returned by the methods above to read.
    pub fn heap(&self) -> Ref<Box<[u8]>> {
        Ref::map(self.inner.borrow(), |heap| &heap.heap)
    }
}

struct MotokoHeapInner {
    /// The heap. This is a boxed slice instead of a vector as growing this wouldn't make sense
    /// (all pointers would have to be updated).
    heap: Box<[u8]>,

    /// Where the dynamic heap starts
    heap_base_offset: usize,

    /// Where the dynamic heap ends, i.e. the heap pointer
    heap_ptr_offset: usize,

    /// Offset of the static root array: an array of pointers below `heap_base`
    static_root_array_offset: usize,

    /// Offset of the closure table. Currently we write a tagged scalar to this location and
    /// effectively skip closure table evacuation.
    closure_table_offset: usize,
}

impl MotokoHeapInner {
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

    /// Set heap pointer
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

    fn new(
        map: &HashMap<ObjectIdx, Vec<ObjectIdx>>,
        roots: &[ObjectIdx],
        gc: GC,
    ) -> MotokoHeapInner {
        // Each object will be 3 words per object + one word for each reference. Static heap will
        // have an array (header + length) with one element, one MutBox for each root.
        let static_heap_size_bytes = (2 + roots.len() + (roots.len() * 2)) * WORD_SIZE;
        let dynamic_heap_size_bytes = {
            let object_headers_words = map.len() * 3;
            let references_words = map.values().map(Vec::len).sum::<usize>();
            let closure_table_words = 1;
            (object_headers_words + references_words + closure_table_words) * WORD_SIZE
        };
        let total_heap_size_bytes = static_heap_size_bytes + dynamic_heap_size_bytes;

        let heap_size = heap_size_for_gc(
            gc,
            static_heap_size_bytes,
            dynamic_heap_size_bytes,
            map.len(),
        );

        let mut heap: Vec<u8> = vec![0; heap_size];

        // Maps `ObjectIdx`s into their offsets in the heap
        let object_addrs: HashMap<ObjectIdx, usize> =
            create_dynamic_heap(map, &mut heap[static_heap_size_bytes..]);

        create_static_heap(roots, &object_addrs, &mut heap[..static_heap_size_bytes]);

        // Add closure table at the end of the heap. Currently closure table is just a scalar.
        let closure_table_offset = static_heap_size_bytes + dynamic_heap_size_bytes - WORD_SIZE;
        write_word(&mut heap, closure_table_offset, 0);

        MotokoHeapInner {
            heap: heap.into_boxed_slice(),
            heap_base_offset: static_heap_size_bytes,
            heap_ptr_offset: total_heap_size_bytes,
            static_root_array_offset: 0,
            closure_table_offset,
        }
    }

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
                "MotokoHeap::grow_memory called: heap_end={:#x}, grow_memory argument={:#x}",
                heap_end, ptr
            );
        }
    }
}

/// Compute the size of the heap to be allocated for the GC test.
fn heap_size_for_gc(
    gc: GC,
    static_heap_size_bytes: usize,
    dynamic_heap_size_bytes: usize,
    n_objects: usize,
) -> usize {
    let total_heap_size_bytes = static_heap_size_bytes + dynamic_heap_size_bytes;
    match gc {
        GC::Copying => {
            let to_space_bytes = dynamic_heap_size_bytes;
            total_heap_size_bytes + to_space_bytes
        }
        GC::MarkCompact => {
            let bitmap_size_bytes = {
                let dynamic_heap_words =
                    Bytes(dynamic_heap_size_bytes as u32).to_words().0 as usize;

                let mark_bit_bytes = (dynamic_heap_words + 7) / 8;

                // The bitmap implementation rounds up to 64-bits to be able to read as many
                // bits as possible in one instruction and potentially skip 64 words in the
                // heap with single 64-bit comparison
                (((mark_bit_bytes + 7) / 8) * 8) + size_of::<Blob>().to_bytes().0 as usize
            };
            // In the worst case the entire heap will be pushed to the mark stack, but in tests
            // we limit the size
            let mark_stack_words = n_objects.clamp(INIT_STACK_SIZE.0 as usize, MAX_MARK_STACK_SIZE)
                + size_of::<Blob>().0 as usize;

            total_heap_size_bytes + bitmap_size_bytes + (mark_stack_words * WORD_SIZE)
        }
    }
}

/// Given a heap description (as a map from objects to objects), and the dynamic part of the heap
/// (as an array), initialize the dynamic heap with objects.
///
/// Returns a mapping from object indices (`ObjectIdx`) to their addresses (see module
/// documentation for "offset" and "address" definitions).
fn create_dynamic_heap(
    refs: &HashMap<ObjectIdx, Vec<ObjectIdx>>,
    dynamic_heap: &mut [u8],
) -> HashMap<ObjectIdx, usize> {
    let heap_start = dynamic_heap.as_ptr() as usize;

    // Maps objects to their addresses
    let mut object_addrs: HashMap<ObjectIdx, usize> = HashMap::new();

    // First pass allocates objects without fields
    {
        let mut heap_offset = 0;
        for (obj, refs) in refs {
            object_addrs.insert(*obj, heap_start + heap_offset);

            // Store object header
            write_word(dynamic_heap, heap_offset, TAG_ARRAY);
            heap_offset += WORD_SIZE;

            // Store length: idx + refs
            write_word(
                dynamic_heap,
                heap_offset,
                u32::try_from(refs.len() + 1).unwrap(),
            );
            heap_offset += WORD_SIZE;

            // Store object value (idx)
            write_word(dynamic_heap, heap_offset, obj << 1);
            heap_offset += WORD_SIZE;

            // Leave space for the fields
            heap_offset += refs.len() * WORD_SIZE;
        }
    }

    // println!("object addresses={:#?}", object_addrs);

    // Second pass adds fields
    for (obj, refs) in refs {
        let obj_offset = object_addrs.get(obj).unwrap() - heap_start;
        for (ref_idx, ref_) in refs.iter().enumerate() {
            // -1 for skewing
            let ref_addr = object_addrs.get(ref_).unwrap().wrapping_sub(1);
            let field_offset = obj_offset + (3 + ref_idx) * WORD_SIZE;
            write_word(dynamic_heap, field_offset, u32::try_from(ref_addr).unwrap());
        }
    }

    object_addrs
}

/// Given a root set (`roots`, may contain duplicates), a mapping from object indices to addresses
/// (`object_addrs`), and the static part of the heap, initialize the static heap with the static
/// root array.
fn create_static_heap(
    roots: &[ObjectIdx],
    object_addrs: &HashMap<ObjectIdx, usize>,
    heap: &mut [u8],
) {
    let root_addresses: Vec<usize> = roots
        .iter()
        .map(|obj| *object_addrs.get(obj).unwrap())
        .collect();

    // Create static root array. Each element of the array is a MutBox pointing to the actual
    // root.
    write_word(heap, 0, TAG_ARRAY);
    write_word(heap, WORD_SIZE, u32::try_from(roots.len()).unwrap());

    // Current offset in the heap for the next static roots array element
    let mut root_addr_offset = 2 * WORD_SIZE;

    // Current offset in the heap for the MutBox of the next root
    let mut mutbox_offset = (2 + roots.len()) * WORD_SIZE;

    for root_address in root_addresses {
        // Add a MutBox for the object
        write_word(heap, mutbox_offset, TAG_MUTBOX);
        write_word(
            heap,
            mutbox_offset + WORD_SIZE,
            u32::try_from(root_address).unwrap().wrapping_sub(1),
        );

        let mutbox_addr = heap.as_ptr() as usize + mutbox_offset;
        write_word(
            heap,
            root_addr_offset,
            u32::try_from(mutbox_addr).unwrap().wrapping_sub(1),
        );

        root_addr_offset += WORD_SIZE;
        mutbox_offset += 2 * WORD_SIZE;
    }
}
