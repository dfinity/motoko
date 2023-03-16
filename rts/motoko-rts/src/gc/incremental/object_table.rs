//! Central object table used by the incremental GC.
//! Maps object ids to the correponding object addresses in the dynamic heap.
//! All references to objects in the dynamic heap are routed through this table.
//! This enables fast moving of objects in the incremental GC by only updating the
//! address of the corresponding object in the table.
//!
//! The table is allocated at the end of the static heap and before the dynamic heap.
//!
//! ┌────────────┬─────────────┬──────────────┬──────────────┬────────────┐
//! │ Rust stack │ Static heap │ Object table | Dynamic heap │ Free space │
//! └────────────┴─────────────┴──────────────┴──────────────┴────────────┘
//!                            ^              ^              ^
//!                            |              |              |
//!                        Table base     Heap base     Heap pointer
//!
//! Heap base is shifted on allocation and growth (shrinking) of the object table.
//! The base of the object table never moves to guarantee immutability of the object
//! ids that are encoded as pointers into the table.
//!
//! The dynamic heap can be organized into generations, e.g. old and young generation
//! with `LAST_HP` splitting both generations. On each GC run, the young generation could
//! be first collected (classically), before the incremental collection of the (extended)
//! old generation continues. Mark stack frames for incremental old generation collection
//! can also be allocated at the end of the old generation (reclaimable by the same
//! incremental GC run).
//!
//! The object table stores an id-to-address translation as an array. Each array element
//! can be used to represent object id with the address of an allocated object stored in
//! the element. Object ids are represented as skewed pointers to the corresponding array
//! element in central object table. Table elements are word-sized.
//!
//!                       Object table
//! Value (skewed)       ┌──────────────┐   
//!    |                 |     ...      |
//!    |   object id     |──────────────|                     Object
//!    └────────────────>|   address    |────────────────>┌─────────────┐
//!                      |──────────────|                 |     ...     |
//!                      |     ...      |                 └─────────────┘
//!                      └──────────────┘
//!
//! Free object ids are stored in a simple stack that is inlined in the array. The top
//! free pointer denotes a free object id, where the element of that id stores the next
//! free object id, and so on. The bottom of the free stack is represented by the sentinel
//! value `FREE_STACK_END`. Insertion and removal from the free id stack is `O(1)` at the
//! top of the stack.
//!
//!                       Object table
//! Top free             ┌──────────────┐   
//!    |                 |     ...      |
//!    |   object id     |──────────────|
//!    └────────────────>| next free id |─────┐
//!                      |─────────────-|     |
//!                      |     ...      |     |
//!                      |─────────────-|     |
//!                ┌─────| next free id |<────┘
//!                |     |─────────────-|
//!                └────>|   free end   |
//!                      └──────────────┘
//!
//! On dynamic allocation of a new object, a free object id has to be popped off the free
//! stack and the address to be recorded in the element. If the free stack is empty and the
//! object table is full, the table is extended (see below).
//!
//! When an object is freed by the GC, the corresponding object id is again pushed back on
//! the free stack.
//!
//! When the garbage collector moves an object, it determines the object id in the header of
//! the object and can then update the address for the corresponding object id in the table.
//! This allows atomic `O(1)` updating of incoming references and thus incremental heap
//! compaction, by moving alive objects, one after the other.
//!
//! Table growth:
//! The runtime system reserves potentially needed free object ids in advance before executing
//! the following function:
//! * On mutator allocation: For the corresponding object.
//! * Starting a GC increment: For the mark stack.
//! * On insertion to the remembered set: For a potential collision node of the inserted entry
//!   and potential table growth.
//! The reservation technique is preferred over lazy table growth on allocation, to avoid
//! object table growth in critical moments, such as during rememebered set insertion,
//! remembered set table growth, and in the middle of a GC increment.
//! When the number of free entries is smaller than the requested reserve, the table is
//! extended at its end, which also shifts the beginning of the dynamic heap space. This involves
//! increasing `HEAP_BASE` and possibly also `LAST_HP` if this is below the new `HEAP_BASE`.
//! Objects blocking the extension of the table can be easily moved to another place, because
//! of the `O(1)` object movement costs by changing their addresses in the table.
//! Notes:
//! * The new `HEAP_BASE` is not necessarily aligned to 32 bytes - this condition is only used
//!   for the other compacting and generational GCs with the mark bitmap.
//! * `LAST_HP` may fall behind the new `HEAP_BASE`, in which case it needs to be increased to the
//!   new `HEAP_BASE`.
//! * If objects are moved to the young generation due to table extension, the object is conservatively
//!   added to the remembered set such that it is promoted back to the old generation on the next GC run.
//!   This is necessary because the moved object may be reachable from other old objects.
//! * The moved object may be marked in the old generation if incremental marking is active. Since it is
//!   added to the remembered set, it will be promoted back to the old generation and marked again
//!   (since the incremental GC is active).
//! * The moved object is always moved to the heap end, such that that incremental compaction will
//!   not miss it.
//!
//! Table shrinking is generally not supported due to the fragmentation of the free slots in table,
//! i.e. free object ids can be spread across the entire table and do not necessarily manifest
//! at table end. If the table end contains a contiguous section with only free ids, it could be
//! shrunk by that size (currently not yet implemented). Otherwise, reassignment of ids would be
//! needed which is not supported as it would require updating fields/array elements storing that id,
//! with entails a full heap/memory scan.
//!
//! Exceptions:
//! * Static objects are not indirected via this table, but their object id directly
//!   store the skewed address in the static heap. This is done because static objects
//!   do not move and the compiler already generates the object ids in the static object
//!   header before this table has been allocated.
//! * Non-incremental GCs. The table is not used and all object ids are represented as
//!   skewed addresses of the corresponding objects.

use core::{mem::size_of, ops::Range};

use crate::{
    constants::WORD_SIZE,
    gc::incremental::{mark_stack::STACK_TABLE_CAPACITY, write_barrier::remember_old_object},
    mem_utils::memcpy_words,
    memory::Memory,
    types::{
        block_size, has_object_header, skew, unskew, Obj, Tag, Value, Words, NULL_OBJECT_ID,
        TAG_FREE_SPACE, TAG_ONE_WORD_FILLER,
    },
};

use super::write_barrier::young_remembered_set_size;

/// Central object table.
pub struct ObjectTable {
    /// Bottom of the table array.
    base: *mut usize,
    /// Number of table entries (words).
    length: usize,
    /// Top of stack for free object ids.
    free_stack: Value,
    /// Number of free object ids.
    free_count: usize,
}

const FREE_STACK_END: Value = NULL_OBJECT_ID;

impl ObjectTable {
    /// Initialize the new object table at address base with `length` entries.
    /// The memory between `base` and `base + length * WORD` must already be reserved.
    pub fn new(base: *mut usize, length: usize) -> ObjectTable {
        debug_assert!(length > 0);
        debug_assert_eq!(base as u32 % WORD_SIZE, 0);
        let mut table = ObjectTable {
            base,
            length,
            free_stack: FREE_STACK_END,
            free_count: 0,
        };
        table.add_free_range(0..length);
        table
    }

    /// Base address of the object table`.
    pub fn base(&self) -> usize {
        self.base as usize
    }

    /// End address of the object table, equals `HEAP_BASE` (except when heap base
    /// is initially aligned to 32 bytes).
    pub fn end(&self) -> usize {
        unsafe { self.base.add(self.length) as usize }
    }

    fn add_free_range(&mut self, range: Range<usize>) {
        debug_assert!(range.start <= range.end);
        self.free_count += range.end - range.start;
        for index in range.rev() {
            let object_id = self.index_to_object_id(index);
            self.push_free_id(object_id);
        }
    }

    /// Allocate a new object id and associate the object's address.
    /// Free object ids must be reserved in advance.
    pub fn new_object_id(&mut self, address: usize) -> Value {
        debug_assert!(address >= self.end());
        assert!(self.free_count > 0);
        self.free_count -= 1;
        let object_id = self.pop_free_id();
        debug_assert!(address >= self.end()); // Table did not grow to this address.
        self.write_element(object_id, address);
        object_id
    }

    /// The garbage collector frees object ids of discarded objects.
    pub fn free_object_id(&mut self, object_id: Value) {
        self.push_free_id(object_id);
        self.free_count += 1;
    }

    /// Retrieve the object address for a given object id.
    pub fn get_object_address(&self, object_id: Value) -> usize {
        self.read_element(object_id)
    }

    /// Record that an object obtained a new address.
    pub fn move_object(&self, object_id: Value, new_address: usize) {
        debug_assert!(self.read_element(object_id) >= self.end());
        debug_assert_eq!(new_address % WORD_SIZE as usize, 0);
        debug_assert!(new_address >= self.end());
        self.write_element(object_id, new_address);
    }

    fn index_to_object_id(&self, index: usize) -> Value {
        unsafe { Value::from_raw(skew(self.base.add(index) as usize) as u32) }
    }

    fn push_free_id(&mut self, object_id: Value) {
        debug_assert!(object_id != FREE_STACK_END);
        self.write_element(object_id, self.free_stack.get_raw() as usize);
        self.free_stack = object_id;
    }

    fn pop_free_id(&mut self) -> Value {
        debug_assert!(self.free_stack != FREE_STACK_END);
        let object_id = self.free_stack;
        self.free_stack = Value::from_raw(self.read_element(object_id) as u32);
        object_id
    }

    fn write_element(&self, object_id: Value, value: usize) {
        unsafe {
            let element = self.get_element(object_id);
            *element = value;
        }
    }

    fn read_element(&self, object_id: Value) -> usize {
        unsafe {
            let entry = self.get_element(object_id);
            *entry
        }
    }

    fn get_element(&self, object_id: Value) -> *mut usize {
        debug_assert!(object_id.is_object_id());
        let element_address = unskew(object_id.get_raw() as usize);
        debug_assert_eq!(element_address % WORD_SIZE as usize, 0);
        debug_assert!(element_address >= self.base as usize);
        debug_assert!(element_address < self.end());
        element_address as *mut usize
    }

    /// Reserve a minimum number of free object ids, potentially triggering
    /// a table extension (adjusting the heap base, possibly also the last heap pointer,
    /// moving objects, and registering entries to the remembered set)
    pub unsafe fn reserve<M: Memory>(&mut self, mem: &mut M, amount: usize) {
        // Conservative reserve for mark stack tables, aroung 1_400 for a 32-bit address space.
        const MARK_STACK_RESERVE: usize = usize::MAX / STACK_TABLE_CAPACITY / size_of::<Obj>();
        // Reserve for inserting an element to the remembered set while extending the object table.
        let mut required = amount + young_remembered_set_size() + MARK_STACK_RESERVE;
        // Fixpoint iteration since the object table extension may allocate object ids for remembered set entries.
        while self.free_count < required {
            let request = required - self.free_count;
            const REQUEST_GRANULARITY: usize = 1_000;
            let new_length = self.length + request;
            let rounded_length =
                (new_length + REQUEST_GRANULARITY - 1) / REQUEST_GRANULARITY * REQUEST_GRANULARITY;
            let free_before = self.free_count;
            self.grow_table(mem, rounded_length);
            debug_assert!(self.free_count > free_before); // Ensure progress.
            required = amount + young_remembered_set_size() + MARK_STACK_RESERVE;
        }
    }

    /// Grow the object table by relocating one object at the table end.
    unsafe fn grow_table<M: Memory>(&mut self, mem: &mut M, required_length: usize) {
        debug_assert_eq!(self.end(), mem.get_heap_base());
        let mut new_length = self.length;
        let mut address = self.end();
        while new_length < required_length {
            let size;
            if address < mem.get_heap_pointer() {
                let block = address as *mut Tag;
                size = block_size(block as usize);
                if has_object_header(*block) {
                    // Relocate the object to the end of dynamic heap and make space
                    // for table extension.
                    // Note: The object could even be a blob of the mark stack or the
                    // remembered set. These data structures therefore also reference
                    // their tables via object ids through the object table.
                    let object_id = (block as *mut Obj).object_id();
                    debug_assert_eq!(object_id.get_object_address(), address);
                    let new_address = mem.alloc_words(size);
                    debug_assert!(address < new_address);
                    memcpy_words(new_address, address, size);
                    self.move_object(object_id, new_address);
                    debug_assert_eq!(object_id.get_object_address(), new_address);
                    debug_assert!(new_address >= mem.get_last_heap_pointer());
                    if address < mem.get_last_heap_pointer() {
                        // The object is moved from the old generation to the young generation,
                        // such that it may be reachable from other objects from the old
                        // generation. Therefore, conservatively add it to the remembered set.
                        // Adding to the remembered set will not imply object table growth.
                        remember_old_object(mem, object_id);
                    }
                } else {
                    // Heap-internal free blocks may result from `Blob::shrink()`.
                    debug_assert!(*block == TAG_FREE_SPACE || *block == TAG_ONE_WORD_FILLER);
                }
                new_length += size.as_usize();
            } else {
                debug_assert_eq!(address, mem.get_heap_pointer());
                size = Words((required_length - new_length) as u32);
                mem.alloc_words(size);
                new_length = required_length;
            }
            address += size.to_bytes().as_usize();
            let old_length = self.length;
            self.length = new_length;
            self.add_free_range(old_length..new_length);
        }
        debug_assert!(self.end() > mem.get_heap_base());
        mem.set_heap_base(self.end());
        debug_assert_eq!(mem.get_heap_base(), address);
    }
}
