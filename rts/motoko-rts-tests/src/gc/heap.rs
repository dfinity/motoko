use crate::gc::utils::round_to_alignment;

use super::utils::{check_alignment, make_pointer, make_scalar, write_word, ObjectIdx, WORD_SIZE};

use motoko_rts::constants::ADDRESS_ALIGNMENT;
use motoko_rts::memory::Memory;
use motoko_rts::types::*;

use std::cell::{Ref, RefCell};
use std::convert::TryFrom;
use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};

use motoko_rts::gc::incremental::partitioned_heap::PARTITION_SIZE;

/// Represents Motoko heaps. Reference counted (implements `Clone`) so we can clone and move values
/// of this type to GC callbacks.
#[derive(Clone)]
pub struct MotokoHeap {
    inner: Rc<RefCell<MotokoHeapInner>>,
}

impl Memory for MotokoHeap {
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        self.inner.borrow_mut().alloc_words(n)
    }

    unsafe fn grow_memory(&mut self, ptr: usize) {
        self.inner.borrow_mut().grow_memory(ptr);
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
        map: &[(ObjectIdx, Vec<ObjectIdx>)],
        roots: &[ObjectIdx],
        continuation_table: &[ObjectIdx],
    ) -> MotokoHeap {
        MotokoHeap {
            inner: Rc::new(RefCell::new(MotokoHeapInner::new(
                map,
                roots,
                continuation_table,
            ))),
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

    /// Get the beginning of dynamic heap, as an address in the current process
    pub fn heap_base_address(&self) -> usize {
        self.inner.borrow().heap_base_address()
    }

    /// Get the offset of the variable pointing to the static root array.
    pub fn static_root_array_variable_offset(&self) -> usize {
        self.inner.borrow().static_root_array_variable_offset
    }

    /// Get the address of the variable pointing to the static root array.
    pub fn static_root_array_variable_address(&self) -> usize {
        self.inner.borrow().static_root_array_variable_address()
    }

    /// Get the offset of the variable pointing to the continuation table.
    pub fn continuation_table_variable_offset(&self) -> usize {
        self.inner.borrow().continuation_table_variable_offset
    }

    /// Get the address of the variable pointing to the continuation table.
    pub fn continuation_table_variable_address(&self) -> usize {
        self.inner.borrow().continuation_table_variable_address()
    }

    /// Get the offset of the variable pointing to region0.
    pub fn region0_pointer_variable_offset(&self) -> usize {
        self.inner.borrow().region0_pointer_variable_offset
    }

    /// Get the address of the variable pointing to region0
    pub fn region0_pointer_variable_address(&self) -> usize {
        self.inner.borrow().region0_pointer_address()
    }

    /// Get the heap as an array. Use `offset` values returned by the methods above to read.
    pub fn heap(&self) -> Ref<Box<[u8]>> {
        Ref::map(self.inner.borrow(), |heap| &heap.heap)
    }

    /// Print heap contents to stdout, for debugging purposes.
    #[allow(unused)]
    pub fn dump(&self) {
        unsafe {
            motoko_rts::debug::dump_heap(
                self.heap_base_address(),
                self.heap_ptr_address(),
                self.static_root_array_variable_address() as *mut Value,
                self.continuation_table_variable_address() as *mut Value,
            );
        }
    }
}

struct MotokoHeapInner {
    /// The heap. This is a boxed slice instead of a vector as growing this wouldn't make sense
    /// (all pointers would have to be updated).
    heap: Box<[u8]>,

    /// Where the dynamic heap starts
    heap_base_offset: usize,

    /// Last dynamic heap end, used for generational gc testing
    _heap_ptr_last: usize,

    /// Where the dynamic heap ends, i.e. the heap pointer
    heap_ptr_offset: usize,

    /// Offset of the static root array.
    ///
    /// Reminder: This location is in static memory and points to an array in the dynamic heap.
    static_root_array_variable_offset: usize,

    /// Offset of the continuation table pointer.
    ///
    /// Reminder: this location is in static heap and will have pointer to an array in dynamic
    /// heap.
    continuation_table_variable_offset: usize,

    /// Offset of the region 0 pointer.
    ///
    /// Reminder: this location is in static heap and will have pointer to an array in dynamic
    /// heap.
    region0_pointer_variable_offset: usize,
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

    /// Get the address of the variable pointing to the static root array.
    fn static_root_array_variable_address(&self) -> usize {
        self.offset_to_address(self.static_root_array_variable_offset)
    }

    /// Get the address of the variable pointing to the continuation table.
    fn continuation_table_variable_address(&self) -> usize {
        self.offset_to_address(self.continuation_table_variable_offset)
    }

    /// Get the address of the region0 pointer
    fn region0_pointer_address(&self) -> usize {
        self.offset_to_address(self.region0_pointer_variable_offset)
    }

    fn new(
        map: &[(ObjectIdx, Vec<ObjectIdx>)],
        roots: &[ObjectIdx],
        continuation_table: &[ObjectIdx],
    ) -> MotokoHeapInner {
        // Check test correctness: an object should appear at most once in `map`
        {
            let heap_objects: FxHashSet<ObjectIdx> = map.iter().map(|(obj, _)| *obj).collect();
            assert_eq!(
                heap_objects.len(),
                map.len(),
                "Invalid test heap: some objects appear multiple times"
            );
        }

        // Three pointers: Static root array, continuation table, and region 0.
        let root_pointers_size_bytes = round_to_alignment(Words(3));

        // The static root is an array (header + length) with one element, one MutBox for each static variable.
        let static_root_set_size_bytes =
            round_to_alignment(size_of::<Array>() + Words(roots.len()))
                + roots.len() * round_to_alignment(size_of::<MutBox>());

        // Each object will have array header plus one word for id per object + one word for each reference.
        let dynamic_heap_size_without_roots = map
            .iter()
            .map(|(_, refs)| round_to_alignment(size_of::<Array>() + Words(1 + refs.len())))
            .sum::<usize>();

        let continuation_table_size =
            round_to_alignment(size_of::<Array>() + Words(continuation_table.len()));

        let region0_size = round_to_alignment(size_of::<Region>());

        let dynamic_heap_size_bytes = dynamic_heap_size_without_roots
            + static_root_set_size_bytes
            + continuation_table_size
            + region0_size;

        let total_heap_size_bytes = root_pointers_size_bytes + dynamic_heap_size_bytes;

        let heap_size = heap_size_for_gc();

        // The Worst-case unalignment w.r.t. 32-byte alignment is 28 (assuming
        // that we have general word alignment). So we over-allocate 28 bytes.
        let mut heap = vec![0u8; heap_size + 28];

        // Align the dynamic heap starts at a 32-byte multiple.
        let realign = (32 - (heap.as_ptr() as usize + root_pointers_size_bytes) % 32) % 32;
        assert_eq!(realign % 4, 0);

        // Maps `ObjectIdx`s into their offsets in the heap.
        let (static_root_array_address, continuation_table_address, region0_address) =
            create_dynamic_heap(
                map,
                roots,
                continuation_table,
                &mut heap[root_pointers_size_bytes + realign..heap_size + realign],
            );

        // Root pointers in static memory space.
        let static_root_array_variable_offset = root_pointers_size_bytes - 3 * WORD_SIZE;
        let continuation_table_variable_offset = root_pointers_size_bytes - 2 * WORD_SIZE;
        let region0_pointer_variable_offset = root_pointers_size_bytes - WORD_SIZE;
        create_static_memory(
            static_root_array_variable_offset,
            continuation_table_variable_offset,
            region0_pointer_variable_offset,
            static_root_array_address,
            continuation_table_address,
            region0_address,
            &mut heap[realign..root_pointers_size_bytes + realign],
        );

        MotokoHeapInner {
            heap: heap.into_boxed_slice(),
            heap_base_offset: root_pointers_size_bytes + realign,
            _heap_ptr_last: root_pointers_size_bytes + realign,
            heap_ptr_offset: total_heap_size_bytes + realign,
            static_root_array_variable_offset: static_root_array_variable_offset + realign,
            continuation_table_variable_offset: continuation_table_variable_offset + realign,
            region0_pointer_variable_offset: region0_pointer_variable_offset + realign,
        }
    }

    unsafe fn alloc_words(&mut self, size: Words<usize>) -> Value {
        let rounded_size = round_object_size(size);
        let mut dummy_memory = DummyMemory {};
        let result = motoko_rts::gc::incremental::get_partitioned_heap()
            .allocate(&mut dummy_memory, rounded_size);
        self.set_heap_ptr_address(result.get_ptr()); // realign on partition changes

        self.linear_alloc_words(rounded_size)
    }

    unsafe fn linear_alloc_words(&mut self, n: Words<usize>) -> Value {
        // Update heap pointer
        let old_hp = self.heap_ptr_address();
        let new_hp = old_hp + n.to_bytes().as_usize();
        self.heap_ptr_offset = new_hp - self.heap.as_ptr() as usize;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);
        check_alignment(old_hp);
        Value::from_ptr(old_hp)
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

struct DummyMemory {}

impl Memory for DummyMemory {
    unsafe fn alloc_words(&mut self, _n: Words<usize>) -> Value {
        unreachable!()
    }

    unsafe fn grow_memory(&mut self, _ptr: usize) {}
}

/// Compute the size of the heap to be allocated for the GC test.
fn heap_size_for_gc() -> usize {
    3 * PARTITION_SIZE
}

/// Given a heap description (as a map from objects to objects), and the dynamic part of the heap
/// (as an array), initialize the dynamic heap with objects.
///
/// Returns a pair containing the address of the static root array and the address of the continuation table.
fn create_dynamic_heap(
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    static_roots: &[ObjectIdx],
    continuation_table: &[ObjectIdx],
    dynamic_heap: &mut [u8],
) -> (usize, usize, usize) {
    let heap_start = dynamic_heap.as_ptr() as usize;

    // Maps objects to their addresses
    let mut object_addrs: FxHashMap<ObjectIdx, usize> = Default::default();

    // First pass allocates objects without fields
    {
        let mut heap_offset = 0;
        for (obj, refs) in refs {
            object_addrs.insert(*obj, heap_start + heap_offset);

            // Store object header
            let address = heap_start + heap_offset;
            check_alignment(address);
            check_alignment(heap_offset);
            write_word(dynamic_heap, heap_offset, TAG_ARRAY);
            heap_offset += WORD_SIZE;

            write_word(dynamic_heap, heap_offset, make_pointer(address)); // forwarding pointer
            heap_offset += WORD_SIZE;

            // Store length: idx + refs
            write_word(
                dynamic_heap,
                heap_offset,
                u32::try_from(refs.len() + 1).unwrap(),
            );
            heap_offset += WORD_SIZE;

            // Store object value (idx)
            write_word(dynamic_heap, heap_offset, make_scalar(*obj));
            heap_offset += WORD_SIZE;

            // Leave space for the fields
            heap_offset += refs.len() * WORD_SIZE;

            align_object_size(dynamic_heap, &mut heap_offset);
        }
    }

    // println!("object addresses={:#?}", object_addrs);

    // Second pass adds fields
    for (obj, refs) in refs {
        let obj_offset = object_addrs.get(obj).unwrap() - heap_start;
        for (ref_idx, ref_) in refs.iter().enumerate() {
            let ref_addr = make_pointer(*object_addrs.get(ref_).unwrap());
            let field_offset = obj_offset
                + (size_of::<Array>() + Words(1 + ref_idx))
                    .to_bytes()
                    .as_usize();

            write_word(dynamic_heap, field_offset, ref_addr);
        }
    }

    // Add the static root table
    let root_section_offset = refs
        .iter()
        .map(|(_, fields)| round_to_alignment(size_of::<Array>() + Words(1 + fields.len())))
        .sum::<usize>();

    let mut heap_offset = root_section_offset;
    let mut root_mutboxes = vec![];
    {
        for root_id in static_roots {
            let mutbox_address = heap_start + heap_offset;
            check_alignment(mutbox_address);
            check_alignment(heap_offset);
            root_mutboxes.push(mutbox_address);
            write_word(dynamic_heap, heap_offset, TAG_MUTBOX);
            heap_offset += WORD_SIZE;

            write_word(dynamic_heap, heap_offset, make_pointer(mutbox_address));
            heap_offset += WORD_SIZE;

            let root_ptr = *object_addrs.get(root_id).unwrap();
            write_word(dynamic_heap, heap_offset, make_pointer(root_ptr));
            heap_offset += WORD_SIZE;

            align_object_size(dynamic_heap, &mut heap_offset);
        }
    }
    let static_root_array_address = heap_start + heap_offset;
    {
        check_alignment(heap_offset);
        write_word(dynamic_heap, heap_offset, TAG_ARRAY);
        heap_offset += WORD_SIZE;

        write_word(
            dynamic_heap,
            heap_offset,
            make_pointer(static_root_array_address),
        );
        heap_offset += WORD_SIZE;

        assert_eq!(static_roots.len(), root_mutboxes.len());
        write_word(dynamic_heap, heap_offset, root_mutboxes.len() as u32);
        heap_offset += WORD_SIZE;

        for mutbox_address in root_mutboxes {
            write_word(dynamic_heap, heap_offset, make_pointer(mutbox_address));
            heap_offset += WORD_SIZE;
        }

        align_object_size(dynamic_heap, &mut heap_offset);
    }

    let continuation_table_address = heap_start + heap_offset;
    {
        check_alignment(heap_offset);
        write_word(dynamic_heap, heap_offset, TAG_ARRAY);
        heap_offset += WORD_SIZE;

        write_word(
            dynamic_heap,
            heap_offset,
            make_pointer(continuation_table_address),
        );
        heap_offset += WORD_SIZE;

        write_word(dynamic_heap, heap_offset, continuation_table.len() as u32);
        heap_offset += WORD_SIZE;

        for idx in continuation_table {
            let idx_ptr = *object_addrs.get(idx).unwrap();
            write_word(dynamic_heap, heap_offset, make_pointer(idx_ptr));
            heap_offset += WORD_SIZE;
        }

        align_object_size(dynamic_heap, &mut heap_offset);
    }

    // Add region0
    let region0_address = heap_start + heap_offset;
    {
        check_alignment(heap_offset);

        write_word(dynamic_heap, heap_offset, TAG_REGION);
        heap_offset += WORD_SIZE;

        write_word(dynamic_heap, heap_offset, make_pointer(region0_address));
        heap_offset += WORD_SIZE;

        // lower part of region id
        write_word(dynamic_heap, heap_offset, 0);
        heap_offset += WORD_SIZE;
        // upper part of region id
        write_word(dynamic_heap, heap_offset, 0);
        heap_offset += WORD_SIZE;
        // zero pages
        write_word(dynamic_heap, heap_offset, 0);
        heap_offset += WORD_SIZE;
        // Simplification: Skip the vector pages blob
        write_word(dynamic_heap, heap_offset, make_scalar(0));

        align_object_size(dynamic_heap, &mut heap_offset);
    }

    (
        static_root_array_address,
        continuation_table_address,
        region0_address,
    )
}

/// Static memory part containing the root pointers.
fn create_static_memory(
    static_root_array_variable_offset: usize,
    continuation_table_variable_offset: usize,
    region0_pointer_variable_offset: usize,
    static_root_array_address: usize,
    continuation_table_address: usize,
    region0_address: usize,
    heap: &mut [u8],
) {
    // Write static array pointer as the third last word in static memory
    write_word(
        heap,
        static_root_array_variable_offset,
        make_pointer(static_root_array_address),
    );

    // Write continuation table pointer as the second last word in static memory
    write_word(
        heap,
        continuation_table_variable_offset,
        make_pointer(continuation_table_address),
    );

    // Write region 0 pointer as the very last word in static memory
    write_word(
        heap,
        region0_pointer_variable_offset,
        make_pointer(region0_address),
    );
}

fn align_object_size(dynamic_heap: &mut [u8], heap_offset: &mut usize) {
    while *heap_offset % ADDRESS_ALIGNMENT.to_bytes().as_usize() != 0 {
        write_word(dynamic_heap, *heap_offset, 0);
        *heap_offset += WORD_SIZE;
    }
}
