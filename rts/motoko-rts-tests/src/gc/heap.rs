use super::utils::{make_pointer, make_scalar, write_word, ObjectIdx, GC, WORD_SIZE};

use motoko_rts::memory::Memory;
use motoko_rts::types::*;

use std::cell::{Ref, RefCell};
use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};
use motoko_rts_macros::*;

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
        _region0_ptr_loc: &[ObjectIdx],
        gc: GC,
    ) -> MotokoHeap {
        MotokoHeap {
            inner: Rc::new(RefCell::new(MotokoHeapInner::new(
                map,
                roots,
                continuation_table,
                gc,
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

    /// Update the heap pointer given as an address in the current process.
    #[non_incremental_gc]
    pub fn set_heap_ptr_address(&self, address: usize) {
        self.inner.borrow_mut().set_heap_ptr_address(address)
    }

    /// Get the last heap pointer, as address in the current process. The address can be used to mutate
    /// the heap.
    #[non_incremental_gc]
    pub fn last_ptr_address(&self) -> usize {
        self.inner.borrow().last_ptr_address()
    }

    /// Update the last heap pointer given as an address in the current process.
    #[non_incremental_gc]
    pub fn set_last_ptr_address(&self, address: usize) {
        self.inner.borrow_mut().set_last_ptr_address(address)
    }

    /// Get the beginning of dynamic heap, as an address in the current process
    pub fn heap_base_address(&self) -> usize {
        self.inner.borrow().heap_base_address()
    }

    /// Get the address of the static root array
    pub fn static_root_array_address(&self) -> usize {
        self.inner.borrow().static_root_array_address()
    }

    /// Get the offset of the continuation table pointer
    pub fn continuation_table_ptr_offset(&self) -> usize {
        self.inner.borrow().continuation_table_ptr_offset
    }

    /// Get the address of the continuation table pointer
    pub fn continuation_table_ptr_address(&self) -> usize {
        self.inner.borrow().continuation_table_ptr_address()
    }

    /// Get the address of the continuation table pointer
    #[incremental_gc]
    pub fn region0_ptr_location(&self) -> usize {
        self.inner.borrow().region0_ptr_address()
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
                Value::from_ptr(self.static_root_array_address()),
                self.continuation_table_ptr_address() as *mut Value,
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

    /// Offset of the static root array: an array of pointers below `heap_base`
    static_root_array_offset: usize,

    /// Offset of the continuation table pointer.
    ///
    /// Reminder: this location is in static heap and will have pointer to an array in dynamic
    /// heap.
    continuation_table_ptr_offset: usize,

    _region0_ptr_location_offset: usize,
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

    /// Get last heap pointer (i.e. where the dynamic heap ends last GC run) in the process's address space
    #[non_incremental_gc]
    fn last_ptr_address(&self) -> usize {
        self.offset_to_address(self._heap_ptr_last)
    }

    /// Set last heap pointer
    #[non_incremental_gc]
    fn set_last_ptr_address(&mut self, address: usize) {
        self._heap_ptr_last = self.address_to_offset(address);
    }

    /// Get static root array address in the process's address space
    fn static_root_array_address(&self) -> usize {
        self.offset_to_address(self.static_root_array_offset)
    }

    /// Get the address of the continuation table pointer
    fn continuation_table_ptr_address(&self) -> usize {
        self.offset_to_address(self.continuation_table_ptr_offset)
    }

    #[incremental_gc]
    fn region0_ptr_address(&self) -> usize {
        self.offset_to_address(self._region0_ptr_location_offset)
    }

    fn new(
        map: &[(ObjectIdx, Vec<ObjectIdx>)],
        roots: &[ObjectIdx],
        continuation_table: &[ObjectIdx],
        gc: GC,
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

        // Each object will have array header plus one word for id per object + one word for each reference. Static heap will
        // have an array (header + length) with one element, one MutBox for each root. +1 for
        // continuation table pointer.
        let static_heap_size_bytes = (size_of::<Array>().as_usize()
            + roots.len()
            + (roots.len() * size_of::<MutBox>().as_usize())
            + 2)
            * WORD_SIZE;

        let dynamic_heap_size_without_continuation_table_bytes = {
            let object_headers_words = map.len() * (size_of::<Array>().as_usize() + 1);
            let references_words = map.iter().map(|(_, refs)| refs.len()).sum::<usize>();
            (object_headers_words + references_words) * WORD_SIZE
        };

        let dynamic_heap_size_bytes = dynamic_heap_size_without_continuation_table_bytes
            + (size_of::<Array>() + Words(continuation_table.len()))
                .to_bytes()
                .as_usize();

        let total_heap_size_bytes = static_heap_size_bytes + dynamic_heap_size_bytes;

        let heap_size = heap_size_for_gc(
            gc,
            static_heap_size_bytes,
            dynamic_heap_size_bytes,
            map.len(),
        );

        const HEAP_ALIGNMENT: usize = usize::BITS as usize;
        // The Worst-case unalignment is one word less than the intended heap alignment
        // (assuming that we have general word alignment). So we over-allocate `HEAP_ALIGNMENT - WORD_SIZE` bytes.
        let mut heap = vec![0u8; heap_size + HEAP_ALIGNMENT - WORD_SIZE];

        // Align the dynamic heap start.
        let realign = (HEAP_ALIGNMENT
            - (heap.as_ptr() as usize + static_heap_size_bytes) % HEAP_ALIGNMENT)
            % HEAP_ALIGNMENT;
        assert_eq!(realign % WORD_SIZE, 0);

        // Maps `ObjectIdx`s into their offsets in the heap
        let object_addrs: FxHashMap<ObjectIdx, usize> = create_dynamic_heap(
            map,
            continuation_table,
            &mut heap[static_heap_size_bytes + realign..heap_size + realign],
        );

        // Closure table pointer is the last word in static heap
        let continuation_table_ptr_offset = static_heap_size_bytes - WORD_SIZE * 2;

        let region0_ptr_location_offset = static_heap_size_bytes - WORD_SIZE;

        create_static_heap(
            roots,
            &object_addrs,
            continuation_table_ptr_offset,
            static_heap_size_bytes + dynamic_heap_size_without_continuation_table_bytes,
            &mut heap[realign..static_heap_size_bytes + realign],
        );

        MotokoHeapInner {
            heap: heap.into_boxed_slice(),
            heap_base_offset: static_heap_size_bytes + realign,
            _heap_ptr_last: static_heap_size_bytes + realign,
            heap_ptr_offset: total_heap_size_bytes + realign,
            static_root_array_offset: realign,
            continuation_table_ptr_offset: continuation_table_ptr_offset + realign,
            _region0_ptr_location_offset: region0_ptr_location_offset + realign,
        }
    }

    #[non_incremental_gc]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        self.linear_alloc_words(n)
    }

    #[incremental_gc]
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value {
        let mut dummy_memory = DummyMemory {};
        let result =
            motoko_rts::gc::incremental::get_partitioned_heap().allocate(&mut dummy_memory, n);
        self.set_heap_ptr_address(result.get_ptr()); // realign on partition changes

        self.linear_alloc_words(n)
    }

    unsafe fn linear_alloc_words(&mut self, n: Words<usize>) -> Value {
        // Update heap pointer
        let old_hp = self.heap_ptr_address();
        let new_hp = old_hp + n.to_bytes().as_usize();
        self.heap_ptr_offset = new_hp - self.heap.as_ptr() as usize;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);
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
#[non_incremental_gc]
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
                let dynamic_heap_bytes = Bytes(dynamic_heap_size_bytes);
                // `...to_words().to_bytes()` below effectively rounds up heap size to word size
                // then gets the bytes
                let dynamic_heap_words = dynamic_heap_bytes.to_words();
                let mark_bit_bytes = dynamic_heap_words.to_bytes();

                // The bitmap implementation rounds up to word-alignment.
                (((mark_bit_bytes.as_usize() + WORD_SIZE - 1) / WORD_SIZE) * WORD_SIZE)
                    + size_of::<Blob>().to_bytes().as_usize()
            };
            // In the worst case the entire heap will be pushed to the mark stack, but in tests
            // we limit the size
            let mark_stack_words = n_objects.clamp(
                motoko_rts::gc::mark_compact::mark_stack::INIT_STACK_SIZE.as_usize(),
                super::utils::MAX_MARK_STACK_SIZE,
            ) + size_of::<Blob>().as_usize();

            total_heap_size_bytes + bitmap_size_bytes + (mark_stack_words * WORD_SIZE)
        }
        GC::Generational => {
            const ROUNDS: usize = 3;
            const REMEMBERED_SET_MAXIMUM_SIZE: usize = 1024 * 1024 * WORD_SIZE;
            let size = heap_size_for_gc(
                GC::MarkCompact,
                static_heap_size_bytes,
                dynamic_heap_size_bytes,
                n_objects,
            );
            size + ROUNDS * REMEMBERED_SET_MAXIMUM_SIZE
        }
    }
}

#[incremental_gc]
fn heap_size_for_gc(
    gc: GC,
    _static_heap_size_bytes: usize,
    _dynamic_heap_size_bytes: usize,
    _n_objects: usize,
) -> usize {
    match gc {
        GC::Incremental => 3 * motoko_rts::gc::incremental::partitioned_heap::PARTITION_SIZE,
    }
}

/// Given a heap description (as a map from objects to objects), and the dynamic part of the heap
/// (as an array), initialize the dynamic heap with objects.
///
/// Returns a mapping from object indices (`ObjectIdx`) to their addresses (see module
/// documentation for "offset" and "address" definitions).
fn create_dynamic_heap(
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    continuation_table: &[ObjectIdx],
    dynamic_heap: &mut [u8],
) -> FxHashMap<ObjectIdx, usize> {
    let incremental = cfg!(feature = "incremental_gc");
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
            write_word(dynamic_heap, heap_offset, TAG_ARRAY);
            heap_offset += WORD_SIZE;

            if incremental {
                write_word(dynamic_heap, heap_offset, make_pointer(address)); // forwarding pointer
                heap_offset += WORD_SIZE;
            }

            // Store length: idx + refs
            write_word(dynamic_heap, heap_offset, refs.len() + 1);
            heap_offset += WORD_SIZE;

            // Store object value (idx)
            write_word(dynamic_heap, heap_offset, make_scalar(*obj));
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
            let ref_addr = make_pointer(*object_addrs.get(ref_).unwrap());
            let field_offset = obj_offset
                + (size_of::<Array>() + Words(1 + ref_idx))
                    .to_bytes()
                    .as_usize();
            write_word(dynamic_heap, field_offset, ref_addr);
        }
    }

    // Add the continuation table
    let n_objects = refs.len();
    // fields+1 for the scalar field (idx)
    let n_fields: usize = refs.iter().map(|(_, fields)| fields.len() + 1).sum();
    let continuation_table_offset =
        (size_of::<Array>() * n_objects).to_bytes().as_usize() + n_fields * WORD_SIZE;

    {
        let mut heap_offset = continuation_table_offset;

        let continuation_table_address = heap_start + heap_offset;
        write_word(dynamic_heap, heap_offset, TAG_ARRAY);
        heap_offset += WORD_SIZE;

        if incremental {
            write_word(
                dynamic_heap,
                heap_offset,
                make_pointer(continuation_table_address),
            );
            heap_offset += WORD_SIZE;
        }

        write_word(dynamic_heap, heap_offset, continuation_table.len());
        heap_offset += WORD_SIZE;

        for idx in continuation_table {
            let idx_ptr = *object_addrs.get(idx).unwrap();
            write_word(dynamic_heap, heap_offset, make_pointer(idx_ptr));
            heap_offset += WORD_SIZE;
        }
    }

    object_addrs
}

/// Given a root set (`roots`, may contain duplicates), a mapping from object indices to addresses
/// (`object_addrs`), and the static part of the heap, initialize the static heap with the static
/// root array.
fn create_static_heap(
    roots: &[ObjectIdx],
    object_addrs: &FxHashMap<ObjectIdx, usize>,
    continuation_table_ptr_offset: usize,
    continuation_table_offset: usize,
    heap: &mut [u8],
) {
    let incremental = cfg!(feature = "incremental_gc");
    let root_addresses: Vec<usize> = roots
        .iter()
        .map(|obj| *object_addrs.get(obj).unwrap())
        .collect();

    // Create static root array. Each element of the array is a MutBox pointing to the actual
    // root.
    let array_addr = heap.as_ptr() as usize;
    let mut offset = 0;
    write_word(heap, offset, TAG_ARRAY);
    offset += WORD_SIZE;

    if incremental {
        write_word(heap, offset, make_pointer(array_addr));
        offset += WORD_SIZE;
    }

    write_word(heap, offset, roots.len());
    offset += WORD_SIZE;

    // Current offset in the heap for the next static roots array element
    let mut root_addr_offset = size_of::<Array>().to_bytes().as_usize();
    assert_eq!(offset, root_addr_offset);

    // Current offset in the heap for the MutBox of the next root
    let mut mutbox_offset = (size_of::<Array>().as_usize() + roots.len()) * WORD_SIZE;

    for root_address in root_addresses {
        // Add a MutBox for the object
        let mutbox_addr = heap.as_ptr() as usize + mutbox_offset;
        let mutbox_ptr = make_pointer(mutbox_addr);

        offset = mutbox_offset;
        write_word(heap, offset, TAG_MUTBOX);
        offset += WORD_SIZE;

        if incremental {
            write_word(heap, offset, mutbox_ptr);
            offset += WORD_SIZE;
        }

        write_word(heap, offset, make_pointer(root_address));
        offset += WORD_SIZE;

        write_word(heap, root_addr_offset, mutbox_ptr);

        root_addr_offset += WORD_SIZE;
        mutbox_offset += size_of::<MutBox>().to_bytes().as_usize();
        assert_eq!(offset, mutbox_offset);
    }

    // Write continuation table pointer as the last word in static heap
    let continuation_table_ptr = continuation_table_offset + heap.as_ptr() as usize;
    write_word(
        heap,
        continuation_table_ptr_offset,
        make_pointer(continuation_table_ptr),
    );
}
