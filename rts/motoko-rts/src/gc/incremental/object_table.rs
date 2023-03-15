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
//! At the end of a GC run, the object table is extended to a conservative size derived
//! from the future heap size for the next scheduled GC run (based on heuristics).
//! This reservation in advance is preferred over lazy table growth on allocation, to avoid
//! object table growth in critical moments, such as during rememebered set insertion,
//! on remembered set table growth, during GC increments, and during assignments when low-level
//! addresses/pointers have been pushed on the call stack, etc..
//! The table is extended at its end, which also shifts the beginning of the dynamic heap space. This involves
//! increasing `HEAP_BASE` and possibly also `LAST_HP` if this is below the new `HEAP_BASE`.
//! Objects blocking the extension of the table can be easily moved to another place, because
//! of the `O(1)` object movement costs by changing their addresses in the table.
//! Notes:
//! * The new `HEAP_BASE` is not necessarily aligned to 32 bytes - this condition is only used
//!   for the other compacting and generational GCs with the mark bitmap.
//! * `LAST_HP` may fall behind the new `HEAP_BASE`, in which case it needs to be increased to the
//!   new `HEAP_BASE`.
//! * The table is only extended if the young generation is empty, in order not to move old objects
//!   to the young generation. This is for simplicity. Otherwise, the moved old object would need to
//!   be added to the remembered set such that it would be promoted back to the old generation
//!   on the next GC run.
//! * The moved object may be marked in the old generation if incremental marking is active. The mark
//!   flag will remain valid with the movement, since it is encoded in a bit of the object header.
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

use core::ops::Range;

use crate::{
    constants::WORD_SIZE,
    gc::incremental::write_barrier::has_young_remembered_set,
    mem_utils::memcpy_words,
    memory::Memory,
    rts_trap_with,
    types::{
        block_size, has_object_header, skew, unskew, Obj, Tag, Value, Words, NULL_OBJECT_ID,
        TAG_FREE_SPACE, TAG_ONE_WORD_FILLER,
    },
};

/// Central object table.
pub struct ObjectTable {
    /// Bottom of the table array.
    base: *mut usize,
    /// Number of table entries (words).
    length: usize,
    /// Top of stack for free object ids.
    free_stack: Value,
    /// Number of free object ids,
    free_count: usize,
}

pub const OBJECT_TABLE_RESERVE: usize = 20_000_000;

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
    /// The object table must be large enough to have free object ids.
    pub fn new_object_id(&mut self, address: usize) -> Value {
        if self.free_count == 0 {
            unsafe {
                rts_trap_with("Full object table");
            }
        }
        self.free_count -= 1;
        debug_assert!(address >= self.end());
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

    /// Reserve space for free object ids at regular intervals.
    /// This may grow the object table.
    /// Only to be called when the young generation is empty and no remembered set is present.
    pub unsafe fn reserve<M: Memory>(&mut self, mem: &mut M) {
        println!(100, "CHECK RESERVE {} {}", self.free_count, self.length);
        if self.free_count < OBJECT_TABLE_RESERVE {
            self.grow(mem, self.length + OBJECT_TABLE_RESERVE - self.free_count);
        }
    }

    /// Grow the object table to a specific size by relocating objects at the table end.
    /// No shrinking.
    unsafe fn grow<M: Memory>(&mut self, mem: &mut M, required_length: usize) {
        println!(100, "GROW TABLE {} {required_length}", required_length - self.length);
        debug_assert!(required_length > self.length);
        // Only the old generation exists. This allows moving objects inside the same generation.
        debug_assert!(!has_young_remembered_set());
        debug_assert_eq!(mem.get_last_heap_pointer(), mem.get_heap_pointer());
        // The table end is equal to the heap base except for the initial 32-byte alignment.
        debug_assert_eq!(self.end() / 32, mem.get_heap_base() / 32);
        debug_assert!(self.end() <= mem.get_heap_base()); // Due to alignment.
        let old_length = self.length;
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
                    let new_address = mem.alloc_words(size);
                    debug_assert!(address < new_address);
                    memcpy_words(new_address, address, size);
                    self.move_object(object_id, new_address);
                    debug_assert!(new_address >= mem.get_last_heap_pointer());
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
        }
        self.length = new_length;
        self.add_free_range(old_length..new_length);
        debug_assert!(self.end() > mem.get_heap_base());
        mem.set_heap_base(self.end());
    }
}
