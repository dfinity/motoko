//! Visited set of objects with their static type.
//! This serves to break cycles in stable functions GC.
//!
//! Contrary to classical garbage collection, the objects are here
//! visited in type-directed manner, such that the same object may need
//! to be re-visited if it is encountered by a different static type.
//!
//! The implementation design is similar to the `remembered_set` of the
//! generational GC, except that it stores the pair of object reference and
//! type id.
//!
//! Hash-set implementation. Linked-list collision handling.
//!
//! Hash function = (ptr / WORD_SIZE) % TABLE_SIZE
//!
//! Hash-table (length N):
//! ----------------------------------
//! | entry[0]   | collision_ptr[0]   |
//! ----------------------------------
//! | entry[1]   | collision_ptr[1]   |
//! ----------------------------------
//! |  ...                           |
//! ----------------------------------
//! | entry[N-1] | collision_ptr[N-1] |
//! ----------------------------------
//!
//! Per collision a new linked list node is appended:
//!
//!                               Collision node
//!                        ------------------------------
//! prev_collision_ptr --> | entry | next_collision_ptr |
//!                        ------------------------------
//!
//! Amortized hash-table growth when exceeding a defined threshold.
//!
//! Growth factor 2 for faster bitwise modulo calculation.
//!
//! NOTE: Remembered set structure is not recorded by write barriers
//! as it is discarded by each GC run.
//!
//! NOTE: The table must be blobs, as their entries must not be
//! analyzed by the GC.

use core::mem::size_of;
use core::ptr::null_mut;

use crate::constants::WORD_SIZE;
use crate::memory::{alloc_blob, Memory};
use crate::types::{block_size, Blob, Bytes, Value, NULL_POINTER, TAG_BLOB_B};

pub struct VisitedSet {
    hash_table: *mut Blob,
    count: usize, // contained entries
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct VisitedValue {
    pub object: Value,
    pub type_id: u64,
}

const NULL_VALUE: VisitedValue = VisitedValue {
    object: NULL_POINTER,
    type_id: 0,
};

#[repr(C)]
struct HashEntry {
    pub value: VisitedValue,
    pub next_collision_ptr: *mut CollisionNode,
}

#[repr(C)]
struct CollisionNode {
    pub header: Blob,
    pub entry: HashEntry,
}

pub struct VisitedSetIterator {
    hash_table: *mut Blob,
    hash_index: usize,
    current_entry: *mut HashEntry,
}

pub const INITIAL_TABLE_LENGTH: usize = 1024;
const GROWTH_FACTOR: usize = 2;
pub const OCCUPATION_THRESHOLD_PERCENT: usize = 65;

impl VisitedSet {
    pub unsafe fn new<M: Memory>(mem: &mut M) -> VisitedSet {
        let hash_table = new_table(mem, INITIAL_TABLE_LENGTH);
        VisitedSet {
            hash_table,
            count: 0,
        }
    }

    pub unsafe fn insert<M: Memory>(&mut self, mem: &mut M, value: VisitedValue) {
        debug_assert_ne!(value, NULL_VALUE);
        let index = self.hash_index(value);
        let entry = table_get(self.hash_table, index);
        if (*entry).value == NULL_VALUE {
            debug_assert_eq!((*entry).next_collision_ptr, null_mut());
            table_set(self.hash_table, index, value);
        } else {
            let mut current = entry;
            while (*current).value != value && (*current).next_collision_ptr != null_mut() {
                let next_node = (*current).next_collision_ptr;
                current = &mut (*next_node).entry;
                debug_assert_ne!((*current).value, NULL_VALUE);
            }
            if (*current).value == value {
                // duplicate
                return;
            }
            debug_assert_ne!((*current).value, NULL_VALUE);
            (*current).next_collision_ptr = new_collision_node(mem, value);
        }
        self.count += 1;
        if self.count > table_length(self.hash_table) * OCCUPATION_THRESHOLD_PERCENT / 100 {
            self.grow(mem);
        }
    }

    pub unsafe fn contains(&self, value: VisitedValue) -> bool {
        debug_assert_ne!(value, NULL_VALUE);
        let index = self.hash_index(value);
        let entry = table_get(self.hash_table, index);
        if (*entry).value != NULL_VALUE {
            let mut current = entry;
            while (*current).value != value && (*current).next_collision_ptr != null_mut() {
                let next_node = (*current).next_collision_ptr;
                current = &mut (*next_node).entry;
                debug_assert_ne!((*current).value, NULL_VALUE);
            }
            if (*current).value == value {
                return true;
            }
        }
        false
    }

    pub unsafe fn hash_index(&self, value: VisitedValue) -> usize {
        // Future optimization: Use bitwise modulo, check for power of 2
        let raw_object = value.object.get_raw();
        let length = table_length(self.hash_table);
        debug_assert_eq!(
            (raw_object / WORD_SIZE) % length,
            (raw_object / WORD_SIZE) & (length - 1)
        );
        let object_hash = raw_object / WORD_SIZE;
        let type_hash = value.type_id as usize;
        let hash = object_hash.wrapping_mul(7).wrapping_add(type_hash);
        hash & (length - 1)
    }

    pub unsafe fn iterate(&self) -> VisitedSetIterator {
        VisitedSetIterator::init(self)
    }

    unsafe fn grow<M: Memory>(&mut self, mem: &mut M) {
        let old_count = self.count;
        let mut iterator = self.iterate();
        let new_length = table_length(self.hash_table) * GROWTH_FACTOR;
        self.hash_table = new_table(mem, new_length);
        self.count = 0;
        while iterator.has_next() {
            let value = iterator.current();
            debug_assert_ne!(value, NULL_VALUE);
            self.insert(mem, value);
            iterator.next();
        }
        assert_eq!(self.count, old_count);
    }
}

impl VisitedSetIterator {
    pub unsafe fn init(remembered_set: &VisitedSet) -> VisitedSetIterator {
        let mut first_entry = table_get(remembered_set.hash_table, 0);
        if (*first_entry).value == NULL_VALUE {
            first_entry = null_mut()
        }
        let mut iterator = VisitedSetIterator {
            hash_table: remembered_set.hash_table,
            hash_index: 0,
            current_entry: first_entry,
        };
        iterator.skip_free();
        iterator
    }

    unsafe fn skip_free(&mut self) {
        let length = table_length(self.hash_table);
        if self.hash_index == length {
            return;
        }
        if self.current_entry != null_mut() {
            debug_assert_ne!((*self.current_entry).value, NULL_VALUE);
            return;
        }
        self.hash_index += 1;
        while self.hash_index < length
            && (*table_get(self.hash_table, self.hash_index)).value == NULL_VALUE
        {
            debug_assert_eq!(
                (*table_get(self.hash_table, self.hash_index)).next_collision_ptr,
                null_mut()
            );
            self.hash_index += 1
        }
        if self.hash_index < length {
            self.current_entry = table_get(self.hash_table, self.hash_index);
            debug_assert_ne!((*self.current_entry).value, NULL_VALUE);
        } else {
            self.current_entry = null_mut();
        }
    }

    pub unsafe fn has_next(&self) -> bool {
        self.current_entry != null_mut()
    }

    pub unsafe fn current(&self) -> VisitedValue {
        debug_assert!(self.has_next());
        debug_assert_ne!((*self.current_entry).value, NULL_VALUE);
        (*self.current_entry).value
    }

    pub unsafe fn next(&mut self) {
        debug_assert!(self.has_next());
        let next_node = (*self.current_entry).next_collision_ptr;
        if next_node == null_mut() {
            self.current_entry = null_mut()
        } else {
            self.current_entry = &mut (*next_node).entry as *mut HashEntry;
        }
        self.skip_free()
    }
}

unsafe fn new_table<M: Memory>(mem: &mut M, size: usize) -> *mut Blob {
    // No post allocation barrier as this RTS-internal blob will be collected by the GC.
    let table = alloc_blob(mem, TAG_BLOB_B, Bytes(size * size_of::<HashEntry>())).as_blob_mut();
    for index in 0..size {
        table_set(table, index, NULL_VALUE);
    }
    table
}

unsafe fn new_collision_node<M: Memory>(mem: &mut M, value: VisitedValue) -> *mut CollisionNode {
    debug_assert_ne!(value, NULL_VALUE);
    // No post allocation barrier as this RTS-internal blob will be collected by the GC.
    let node = alloc_blob(mem, TAG_BLOB_B, Bytes(size_of::<HashEntry>())).as_blob_mut()
        as *mut CollisionNode;
    (*node).entry = HashEntry {
        value,
        next_collision_ptr: null_mut(),
    };
    node
}

unsafe fn table_get(table: *mut Blob, index: usize) -> *mut HashEntry {
    debug_assert_ne!(table, null_mut());
    let entry = (table.payload_addr() as usize + index * size_of::<HashEntry>()) as *mut HashEntry;
    debug_assert!(
        entry as usize + size_of::<HashEntry>()
            <= table as usize + block_size(table as usize).to_bytes().as_usize()
    );
    entry
}

unsafe fn table_set(table: *mut Blob, index: usize, value: VisitedValue) {
    let entry = table_get(table, index);
    (*entry).value = value;
    (*entry).next_collision_ptr = null_mut();
}

unsafe fn table_length(table: *mut Blob) -> usize {
    debug_assert!(table != null_mut());
    debug_assert!(table.len().as_usize() % size_of::<HashEntry>() == 0);
    table.len().as_usize() / size_of::<HashEntry>()
}
