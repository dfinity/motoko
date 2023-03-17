//! Central object table used by the incremental GC.
//! Maps object ids to the correponding object addresses in the dynamic heap.
//! All references to objects in the dynamic heap are routed through this table.
//! This enables fast moving of objects in the incremental GC by only updating the
//! address of the corresponding object in the table. Objects also carry their id
//! in the header to allow fast lookup of the object id by a given object address.
//!
//! The table is allocated in the heap and can also be moved (e.g. when growing).
//! A global pointer denotes the current table location.
//!
//! Allowing relocation of the table is more expensive in terms of object address lookup
//! (extra indirection via the global object table pointer), but allows a significantly
//! simpler implementation, since the table can grow at any time without having to move
//! other objects.
//!
//! The dynamic heap can be organized into generations, e.g. old and young generation
//! with `LAST_HP` splitting both generations. On each GC run, the young generation could
//! be first collected (classically), before the incremental collection of the (extended)
//! old generation continues. Mark stack tables for incremental old generation collection
//! can also be allocated inside the young generation (e.g. because of the write barrier),
//! if they are additionally registered in the remembered set.
//!
//! The object table stores an id-to-address translation as an array. Each array element
//! can be used to represent object id with the address of an allocated object stored in
//! the element. Object ids are encoded as skewed offsets for the corresponding array
//! element in central object table. Table elements are word-sized.
//!
//!             Table pointer
//!                  |
//!                  |    Object table
//! Value (skewed)   └──>┌─────────────┐   
//!    | object id       |     ...     |
//!    | + table pointer |─────────────|                     Object
//!    └───────────────> |   address   |───────────────> ┌─────────────┐
//!    |                 |─────────────|                 |     tag     |
//!    |                 |     ...     |                 |─────────────|
//!    |                 └─────────────┘       ┌─────────|  object id  |
//!    |                                       |         |─────────────|
//!    └───────────────────────────────────────┘         |     ...     |
//!                                                      └─────────────┘
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
//! Special GC root:
//! The object table constitutes an additional special GC root. It is also moved by the GC, which
//! additionally requires updating the table pointer. The table has no object id (`NULL_OBJECT_ID`
//! in its header).
//!
//! Table growth:
//! The table is extended to exponentially large table at a different location in the heap, and
//! the heap pointer is updated. Pre-amortized growth could be used in future to obtain `O(1)`
//! worst-case per insertion.
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

use core::{ops::Range, ptr::null_mut};

use crate::{
    constants::WORD_SIZE,
    memory::Memory,
    rts_trap_with,
    types::{size_of, skew, unskew, Blob, Value, Words, NULL_OBJECT_ID, TAG_BLOB},
};

/// Current pointer to the object table. Constitutes a special GC root.
pub static mut OBJECT_TABLE: *mut ObjectTable = null_mut();

/// TODO: Remove once also the static objects are indirected via the object table.
pub static mut HEAP_BASE: usize = 0;

pub unsafe fn initialize_object_table<M: Memory>(mem: &mut M) {
    HEAP_BASE = mem.get_heap_base();
    const INITIAL_TABLE_SIZE: usize = 10_000;
    OBJECT_TABLE = ObjectTable::new(mem, INITIAL_TABLE_SIZE);
}

/// Central object table.
#[repr(C)]
pub struct ObjectTable {
    header: Blob,
    /// Top of stack for free object ids.
    free_stack: Value,
}

const FREE_STACK_END: Value = NULL_OBJECT_ID;

impl ObjectTable {
    /// Allocate a new object table with `size` entries.
    pub unsafe fn new<M: Memory>(mem: &mut M, size: usize) -> *mut ObjectTable {
        if size >= usize::MAX / 2 {
            // Bit 31 of the object id is used to encode mark bit in the object header.
            rts_trap_with("Too large object table");
        }
        debug_assert!(size > 0);
        let raw_size = size_of::<ObjectTable>() + Words(size as u32);
        let address = mem.alloc_words(raw_size);
        let table = address as *mut ObjectTable;
        (*table).header.header.tag = TAG_BLOB;
        (*table).header.header.initialize_id(NULL_OBJECT_ID);
        (*table).header.len = (raw_size - size_of::<Blob>()).to_bytes();
        (*table).free_stack = FREE_STACK_END;
        table.add_free_range(0..size);
        table
    }

    /// Number of entries.
    pub unsafe fn size(self: *const ObjectTable) -> usize {
        debug_assert_eq!((*self).header.len.as_u32() % WORD_SIZE, 0);
        // Subtract the `free_stack` word.
        (*self).header.len.as_usize() / WORD_SIZE as usize - 1
    }

    /// Address to the first table entry.
    pub unsafe fn entries(self: *mut ObjectTable) -> *mut Value {
        // Skip the declared `ObjectTable` header (Blob header with `free_stack` word).
        self.offset(1) as *mut Value
    }

    unsafe fn add_free_range(self: *mut ObjectTable, range: Range<usize>) {
        debug_assert!(range.start <= range.end);
        let mut index = range.end;
        while index > range.start {
            index -= 1;
            let object_id = self.index_to_object_id(index);
            self.push_free_id(object_id);
        }
    }

    /// Allocate a new object id and associate the object's address.
    pub unsafe fn new_object_id(self: *mut ObjectTable, address: usize) -> Value {
        let object_id = self.pop_free_id();
        self.write_element(object_id, address);
        object_id
    }

    /// The garbage collector frees object ids of discarded objects.
    pub unsafe fn free_object_id(self: *mut ObjectTable, object_id: Value) {
        self.push_free_id(object_id);
    }

    /// Retrieve the object address for a given object id.
    pub unsafe fn get_object_address(self: *mut ObjectTable, object_id: Value) -> usize {
        self.read_element(object_id)
    }

    /// Record that an object obtained a new address.
    pub unsafe fn move_object(self: *mut ObjectTable, object_id: Value, new_address: usize) {
        debug_assert_eq!(new_address % WORD_SIZE as usize, 0);
        self.write_element(object_id, new_address);
    }

    unsafe fn index_to_object_id(self: *const ObjectTable, index: usize) -> Value {
        Value::from_raw(skew(index * WORD_SIZE as usize) as u32)
    }

    unsafe fn push_free_id(self: *mut ObjectTable, object_id: Value) {
        debug_assert!(object_id != FREE_STACK_END);
        debug_assert!(object_id != NULL_OBJECT_ID);
        self.write_element(object_id, (*self).free_stack.get_raw() as usize);
        (*self).free_stack = object_id;
    }

    unsafe fn pop_free_id(self: *mut ObjectTable) -> Value {
        if (*self).free_stack == FREE_STACK_END {
            rts_trap_with("Full object table"); // TODO: Grow
        }
        debug_assert!((*self).free_stack != FREE_STACK_END);
        let object_id = (*self).free_stack;
        (*self).free_stack = Value::from_raw(self.read_element(object_id) as u32);
        object_id
    }

    unsafe fn write_element(self: *mut ObjectTable, object_id: Value, value: usize) {
        let element = self.get_element(object_id);
        *element = value;
    }

    unsafe fn read_element(self: *mut ObjectTable, object_id: Value) -> usize {
        let entry = self.get_element(object_id);
        *entry
    }

    unsafe fn get_element(self: *mut ObjectTable, object_id: Value) -> *mut usize {
        debug_assert!(object_id.is_object_id());
        debug_assert!(object_id != NULL_OBJECT_ID);
        let offset = unskew(object_id.get_raw() as usize);
        debug_assert_eq!(offset % WORD_SIZE as usize, 0);
        debug_assert!(offset < self.size() * WORD_SIZE as usize);
        let address = self.entries() as usize + offset;
        address as *mut usize
    }
}
