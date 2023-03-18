use super::utils::{
    make_object_id, make_scalar, write_word, ObjectIdx, GC, MAX_MARK_STACK_SIZE, WORD_SIZE,
};

use motoko_rts::gc::incremental::object_table::{ObjectTable, OBJECT_TABLE, FREE_STACK_END, OBJECT_TABLE_ID};
use motoko_rts::gc::mark_compact::mark_stack::INIT_STACK_SIZE;
use motoko_rts::memory::{Memory, Roots};
use motoko_rts::types::*;

use std::cell::{Ref, RefCell};
use std::convert::TryFrom;
use std::ptr::null_mut;
use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};

/// Represents Motoko heaps. Reference counted (implements `Clone`) so we can clone and move values
/// of this type to GC callbacks.
#[derive(Clone)]
pub struct MotokoHeap {
    inner: Rc<RefCell<MotokoHeapInner>>,
}

impl Memory for MotokoHeap {
    fn get_roots(&self) -> Roots {
        Roots {
            static_roots: self.inner.borrow().static_root_array_id,
            continuation_table_location: self.inner.borrow().continuation_table_ptr_address()
                as *mut Value,
        }
    }

    fn get_heap_base(&self) -> usize {
        self.inner.borrow().heap_base_address()
    }

    fn get_last_heap_pointer(&self) -> usize {
        self.inner.borrow().last_ptr_address()
    }

    fn get_heap_pointer(&self) -> usize {
        self.inner.borrow().heap_ptr_address()
    }

    unsafe fn shrink_heap(&mut self, new_heap_pointer: usize) {
        let mut inner = self.inner.borrow_mut();
        inner.set_heap_ptr_address(new_heap_pointer);
        inner.set_last_ptr_address(new_heap_pointer);
    }

    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize {
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
        map: &[(ObjectIdx, Vec<ObjectIdx>)],
        roots: &[ObjectIdx],
        continuation_table: &[ObjectIdx],
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

    /// Get the beginning of dynamic heap, as an address in the current process
    pub fn heap_base_address(&self) -> usize {
        self.inner.borrow().heap_base_address()
    }

    // Get static root array object id.
    pub fn static_root_array_id(&self) -> Value {
        self.inner.borrow().static_root_array_id
    }

    /// Get the offset of the continuation table pointer
    pub fn continuation_table_ptr_offset(&self) -> usize {
        self.inner.borrow().continuation_table_ptr_offset
    }

    /// Get the address of the continuation table pointer
    pub fn continuation_table_ptr_address(&self) -> usize {
        self.inner.borrow().continuation_table_ptr_address()
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
                self.heap_base_address() as u32,
                self.heap_ptr_address() as u32,
                self.static_root_array_id(),
                self.continuation_table_ptr_address() as *mut Value,
            );
        }
    }
}

const OBJECT_TABLE_SIZE: usize = 1000;

struct MotokoHeapInner {
    /// The heap. This is a boxed slice instead of a vector as growing this wouldn't make sense
    /// (all pointers would have to be updated).
    heap: Box<[u8]>,

    /// Where the dynamic heap starts
    heap_base_offset: usize,

    /// Last dynamic heap end, used for generational gc testing
    heap_ptr_last: usize,

    /// Where the dynamic heap ends, i.e. the heap pointer
    heap_ptr_offset: usize,

    /// Object id of the static root array.
    static_root_array_id: Value,

    /// Offset of the continuation table pointer.
    ///
    /// Reminder: this location is in static heap and will have pointer to an array in dynamic
    /// heap.
    continuation_table_ptr_offset: usize,
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
    fn last_ptr_address(&self) -> usize {
        self.offset_to_address(self.heap_ptr_last)
    }

    /// Set last heap pointer
    fn set_last_ptr_address(&mut self, address: usize) {
        self.heap_ptr_last = self.address_to_offset(address);
    }

    /// Get the address of the continuation table pointer
    fn continuation_table_ptr_address(&self) -> usize {
        self.offset_to_address(self.continuation_table_ptr_offset)
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
            unsafe {
                assert_eq!(OBJECT_TABLE, null_mut());
            }
        }

        // Each object will have array header plus one word for id per object + one word for each reference. Static heap will
        // have an array (header + length) with one element, one MutBox for each root. +1 for
        // continuation table pointer.
        let static_heap_size_bytes = (size_of::<Array>().as_usize()
            + roots.len()
            + (roots.len() * size_of::<MutBox>().as_usize())
            + 1)
            * WORD_SIZE;

        let use_object_table = gc == GC::Incremental;

        let object_table_raw_size = if use_object_table {
            (size_of::<ObjectTable>() + Words(OBJECT_TABLE_SIZE as u32))
                .to_bytes()
                .as_usize()
        } else {
            0
        };

        let dynamic_heap_size_without_continuation_table_bytes = {
            let object_headers_words = map.len() * (size_of::<Array>().as_usize() + 1);
            let references_words = map.iter().map(|(_, refs)| refs.len()).sum::<usize>();
            (object_headers_words + references_words) * WORD_SIZE
        } + object_table_raw_size;

        let dynamic_heap_size_bytes = dynamic_heap_size_without_continuation_table_bytes
            + (size_of::<Array>() + Words(continuation_table.len() as u32))
                .to_bytes()
                .as_usize();

        let total_heap_size_bytes = static_heap_size_bytes + dynamic_heap_size_bytes;

        let heap_size = heap_size_for_gc(
            gc,
            static_heap_size_bytes,
            dynamic_heap_size_bytes,
            map.len(),
        );

        // The Worst-case unalignment w.r.t. 32-byte alignment is 28 (assuming
        // that we have general word alignment). So we over-allocate 28 bytes.
        let mut heap = vec![0u8; heap_size + 28];

        // Align the dynamic heap starts at a 32-byte multiple.
        let realign = (32 - (heap.as_ptr() as usize + static_heap_size_bytes) % 32) % 32;
        assert_eq!(realign % 4, 0);

        let structure = create_dynamic_heap(
            OBJECT_TABLE_SIZE,
            map,
            continuation_table,
            &mut heap[static_heap_size_bytes + realign..heap_size + realign],
        );

        // Closure table pointer is the last word in static heap
        let continuation_table_ptr_offset = static_heap_size_bytes - WORD_SIZE;
        let static_root_array_id = create_static_heap(
            roots,
            &structure,
            continuation_table_ptr_offset,
            &mut heap[realign..static_heap_size_bytes + realign],
        );

        MotokoHeapInner {
            heap: heap.into_boxed_slice(),
            heap_base_offset: static_heap_size_bytes + realign,
            heap_ptr_last: static_heap_size_bytes + realign,
            heap_ptr_offset: total_heap_size_bytes + realign,
            static_root_array_id,
            continuation_table_ptr_offset: continuation_table_ptr_offset + realign,
        }
    }

    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.heap_ptr_address();
        let new_hp = old_hp + bytes.as_usize();
        self.heap_ptr_offset = new_hp - self.heap.as_ptr() as usize;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        old_hp
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
                let dynamic_heap_bytes = Bytes(dynamic_heap_size_bytes as u32);
                // `...to_words().to_bytes()` below effectively rounds up heap size to word size
                // then gets the bytes
                let dynamic_heap_words = dynamic_heap_bytes.to_words();
                let mark_bit_bytes = dynamic_heap_words.to_bytes();

                // The bitmap implementation rounds up to 64-bits to be able to read as many
                // bits as possible in one instruction and potentially skip 64 words in the
                // heap with single 64-bit comparison
                (((mark_bit_bytes.as_u32() + 7) / 8) * 8) + size_of::<Blob>().to_bytes().as_u32()
            };
            // In the worst case the entire heap will be pushed to the mark stack, but in tests
            // we limit the size
            let mark_stack_words = n_objects.clamp(INIT_STACK_SIZE.as_usize(), MAX_MARK_STACK_SIZE)
                + size_of::<Blob>().as_usize();

            total_heap_size_bytes + bitmap_size_bytes as usize + (mark_stack_words * WORD_SIZE)
        }
        GC::Generational | GC::Incremental => {
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

struct DynamicHeapStructure {
    object_ids: FxHashMap<ObjectIdx, Value>,
    continuation_table_id: Value,
}

/// Given a heap description (as a map from objects to objects), and the dynamic part of the heap
/// (as an array), initialize the dynamic heap with objects.
///
/// Returns a mapping from object indices (`ObjectIdx`) to their object ids (`Value`).
fn create_dynamic_heap(
    object_table_size: usize,
    refs: &[(ObjectIdx, Vec<ObjectIdx>)],
    continuation_table: &[ObjectIdx],
    dynamic_heap: &mut [u8],
) -> DynamicHeapStructure {
    assert_eq!(object_table_size % WORD_SIZE, 0);

    let heap_start = dynamic_heap.as_ptr() as usize;
    let mut heap_offset = 0;
    if object_table_size > 0 {
        write_word(dynamic_heap, heap_offset, TAG_BLOB);
        write_word(
            dynamic_heap,
            heap_offset + WORD_SIZE,
            OBJECT_TABLE_ID.get_raw(),
        );
        heap_offset += 2 * WORD_SIZE;
        write_word(
            dynamic_heap,
            heap_offset,
            ((object_table_size + 1) * WORD_SIZE) as u32,
        );
        heap_offset += WORD_SIZE;
        write_word(
            dynamic_heap,
            heap_offset,
            2 * WORD_SIZE as u32, // Top free stack
        );
        heap_offset += WORD_SIZE;
        write_word(
            dynamic_heap,
            heap_offset,
            0,
        );
        heap_offset += WORD_SIZE;
        write_word(
            dynamic_heap,
            heap_offset,
            0,
        );
        heap_offset += WORD_SIZE;
        for index in 1..object_table_size {
            let next_free = skew(index * WORD_SIZE as usize) as u32;
            write_word(dynamic_heap, heap_offset, next_free);
            heap_offset += WORD_SIZE;
        }
        write_word(dynamic_heap, heap_offset, FREE_STACK_END.get_raw());
        heap_offset += WORD_SIZE;
        unsafe {
            assert_eq!(OBJECT_TABLE, null_mut());
            OBJECT_TABLE = heap_start as *mut ObjectTable;
        }
    }

    // Maps objects to their addresses
    let mut object_ids: FxHashMap<ObjectIdx, Value> = Default::default();

    // First pass allocates objects without fields
    {
        for (obj, refs) in refs {
            let address = u32::try_from(heap_start + heap_offset).unwrap();
            let object_id = make_object_id(address);
            object_ids.insert(*obj, object_id);

            // Store object header
            write_word(dynamic_heap, heap_offset, TAG_ARRAY);
            write_word(dynamic_heap, heap_offset + WORD_SIZE, object_id.get_raw());
            heap_offset += 2 * WORD_SIZE;

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
        }
    }

    // Second pass adds fields
    for (obj, refs) in refs {
        let object_id = *object_ids.get(obj).unwrap();
        let object_address = unsafe { object_id.get_object_address() };
        let obj_offset = object_address - heap_start;
        for (ref_idx, ref_) in refs.iter().enumerate() {
            let ref_id = *object_ids.get(ref_).unwrap();
            let field_offset = obj_offset
                + (size_of::<Array>() + Words(1 + ref_idx as u32))
                    .to_bytes()
                    .as_usize();
            write_word(dynamic_heap, field_offset, ref_id.get_raw());
        }
    }

    // Add the continuation table
    let n_objects = refs.len();
    // fields+1 for the scalar field (idx)
    let n_fields: usize = refs.iter().map(|(_, fields)| fields.len() + 1).sum();
    let continuation_table_offset = object_table_size
        + (size_of::<Array>() * n_objects as u32)
            .to_bytes()
            .as_usize()
        + n_fields * WORD_SIZE;

    let continuation_table_id =
        make_object_id(u32::try_from(heap_start + continuation_table_offset).unwrap());

    {
        let mut heap_offset = continuation_table_offset;
        write_word(dynamic_heap, continuation_table_offset, TAG_ARRAY);
        write_word(
            dynamic_heap,
            continuation_table_offset + WORD_SIZE,
            continuation_table_id.get_raw(),
        );
        heap_offset += 2 * WORD_SIZE;

        write_word(dynamic_heap, heap_offset, continuation_table.len() as u32);
        heap_offset += WORD_SIZE;

        for idx in continuation_table {
            let continuation_id = *object_ids.get(idx).unwrap();
            write_word(dynamic_heap, heap_offset, continuation_id.get_raw());
            heap_offset += WORD_SIZE;
        }
    }

    DynamicHeapStructure {
        object_ids,
        continuation_table_id,
    }
}

/// Given a root set (`roots`, may contain duplicates), a mapping from object indices (`ObjectIdx`)
/// to their object ids (`Value`), and the static part of the heap, initialize the static heap with
/// the static root array.
fn create_static_heap(
    roots: &[ObjectIdx],
    structure: &DynamicHeapStructure,
    continuation_table_ptr_offset: usize,
    heap: &mut [u8],
) -> Value {
    let root_ids: Vec<Value> = roots
        .iter()
        .map(|obj| *structure.object_ids.get(obj).unwrap())
        .collect();

    // Create static root array. Each element of the array is a MutBox pointing to the actual
    // root.
    let array_addr = u32::try_from(heap.as_ptr() as usize).unwrap();
    write_word(heap, 0, TAG_ARRAY);
    let root_array_id = make_object_id(array_addr);
    write_word(heap, WORD_SIZE, root_array_id.get_raw());
    write_word(heap, 2 * WORD_SIZE, u32::try_from(roots.len()).unwrap());

    // Current offset in the heap for the next static roots array element
    let mut root_addr_offset = size_of::<Array>().to_bytes().as_usize();

    // Current offset in the heap for the MutBox of the next root
    let mut mutbox_offset = (size_of::<Array>().as_usize() + roots.len()) * WORD_SIZE;

    for root_id in root_ids {
        // Add a MutBox for the object
        let mutbox_addr = heap.as_ptr() as usize + mutbox_offset;

        let mutbox_id = make_object_id(u32::try_from(mutbox_addr).unwrap());

        write_word(heap, mutbox_offset, TAG_MUTBOX);
        write_word(heap, mutbox_offset + WORD_SIZE, mutbox_id.get_raw());
        write_word(heap, mutbox_offset + 2 * WORD_SIZE, root_id.get_raw());

        write_word(heap, root_addr_offset, mutbox_id.get_raw());

        root_addr_offset += WORD_SIZE;
        mutbox_offset += size_of::<MutBox>().to_bytes().as_usize();
    }

    // Write continuation table pointer as the last word in static heap
    write_word(
        heap,
        continuation_table_ptr_offset,
        structure.continuation_table_id.get_raw(),
    );

    root_array_id
}
