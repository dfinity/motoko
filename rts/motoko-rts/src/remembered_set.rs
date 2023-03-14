//! Remembered set.
//! Used by both the generational GC and the incremental GC.
//! Serves for recording entries trapped by the write barrier.
//!
//! The type of entries depends on the GC:
//! * Generational GC: Written locations in the old generation,
//!   i.e. addresses of fields or array elements, unskewed.
//! * Incremental GC: Ids of objects in the young generation,
//!   skewed.
//!
//! Hash-set implementation. Linked-list collision handling.
//!
//! Hash function = (value.get_raw() / WORD_SIZE) % TABLE_SIZE
//!
//! Hash-table (length N):
//! ┌────────────┬─────────────────────┐
//! | entry[0]   | collision_list[0]   |
//! |────────────|─────────────────────|
//! | entry[1]   | collision_list[1]   |
//! |────────────|─────────────────────|
//! |  ...                             |
//! |────────────|─────────────────────|
//! | entry[N-1] | collision_list[N-1] |
//! └────────────┴─────────────────────┘
//!
//! Per collision a new linked list node is appended:
//!
//!                          Collision node
//!                    ┌───────┬────────────────┐
//! prev_collision --> | entry | next_collision |
//!                    └───────┴────────────────┘
//!
//! Amortized hash-table growth when exceeding a defined threshold.
//!
//! Growth factor 2 for faster bitwise modulo calculation.
//!
//! NOTES:
//! * Remembered set structure updates are not recorded by write
//!   barriers as it is discarded by each GC run.
//! * The table must be blobs, as their entries must not be visited and
//!   during GC marking.
//! * The internal blobs used to store the remembered set (hash-table and
//!   collision nodes) are referenced through object ids, to allow their
//!   relocation when it is needed to extend the object table.
//! * In the case of incremental GC, the recorded entries are object ids,
//!   because recorded objects can be relocated, in particular also
//!   young objects in the case of object table growth inside the young
//!   generation.
//! * The object ids required by remembered set insertion and hash table
//!   growth will be reserved in advance to prevent reentrant calls to
//!   remembered set operations. This is because object id allocation
//!   may trigger an object table extension that again registers
//!   moved old objects in the remembered set. Also the remembered set
//!   hash table extension could trigger object table extension which
//!   would call back to the remembered set.

use core::mem::size_of;
use core::ptr::null_mut;

use crate::constants::WORD_SIZE;
use crate::memory::{alloc_blob_internal, Memory};
use crate::types::{block_size, Blob, Bytes, Value, NULL_OBJECT_ID};

pub struct RememberedSet {
    hash_table: Value,
    count: u32, // contained entries
}

#[repr(C)]
struct HashEntry {
    pub value: Value,
    pub next_collision: Value,
}

#[repr(C)]
struct CollisionNode {
    pub header: Blob,
    pub entry: HashEntry,
}

pub struct RememberedSetIterator {
    hash_table: Value,
    hash_index: u32,
    use_current_entry: bool,
    current_collision: Value,
}

pub const INITIAL_TABLE_LENGTH: u32 = 1024;
const GROWTH_FACTOR: u32 = 2;
pub const OCCUPATION_THRESHOLD_PERCENT: u32 = 65;

impl RememberedSet {
    pub unsafe fn new<M: Memory>(mem: &mut M) -> RememberedSet {
        let hash_table = new_table(mem, INITIAL_TABLE_LENGTH);
        RememberedSet {
            hash_table,
            count: 0,
        }
    }

    pub unsafe fn insert<M: Memory>(&mut self, mem: &mut M, value: Value) {
        debug_assert!(!is_null_value(value));
        let hash_table = self.hash_table.as_blob_mut();
        let index = Self::hash_index(hash_table, value);
        let entry = table_get(hash_table, index);
        if is_null_value((*entry).value) {
            debug_assert!(is_null_value((*entry).next_collision));
            table_set(hash_table, index, value);
        } else {
            let mut current = entry;
            while (*current).value.get_raw() != value.get_raw()
                && !is_null_value((*current).next_collision)
            {
                let next_node = as_collision((*current).next_collision);
                current = &mut (*next_node).entry;
                debug_assert!(!is_null_value((*current).value));
            }
            if (*current).value.get_raw() == value.get_raw() {
                // duplicate
                return;
            }
            debug_assert!(!is_null_value((*current).value));
            (*current).next_collision = new_collision_node(mem, value);
        }
        self.count += 1;
        if self.count > table_length(hash_table) * OCCUPATION_THRESHOLD_PERCENT / 100 {
            self.grow(mem);
        }
    }

    // Only used for assertions (barrier coverage check).
    pub unsafe fn contains(&self, value: Value) -> bool {
        debug_assert!(!is_null_value(value));
        let hash_table = self.hash_table.as_blob_mut();
        let index = Self::hash_index(hash_table, value);
        let entry = table_get(hash_table, index);
        if !is_null_value((*entry).value) {
            let mut current = entry;
            while (*current).value.get_raw() != value.get_raw()
                && !is_null_value((*current).next_collision)
            {
                let next_node = as_collision((*current).next_collision);
                current = &mut (*next_node).entry;
                debug_assert!(!is_null_value((*current).value));
            }
            if (*current).value.get_raw() == value.get_raw() {
                return true;
            }
        }
        false
    }

    pub unsafe fn hash_index(hash_table: *mut Blob, value: Value) -> u32 {
        // Future optimization: Use bitwise modulo, check for power of 2
        let raw = value.get_raw();
        let length = table_length(hash_table);
        debug_assert_eq!((raw / WORD_SIZE) % length, (raw / WORD_SIZE) & (length - 1));
        (raw / WORD_SIZE) & (length - 1)
    }

    pub unsafe fn iterate(&self) -> RememberedSetIterator {
        RememberedSetIterator::init(self)
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    unsafe fn grow<M: Memory>(&mut self, mem: &mut M) {
        let old_count = self.count;
        let mut iterator = self.iterate();
        let new_length = table_length(self.hash_table.as_blob_mut()) * GROWTH_FACTOR;
        self.hash_table = new_table(mem, new_length);
        self.count = 0;
        while iterator.has_next() {
            let value = iterator.current();
            debug_assert!(!is_null_value(value));
            self.insert(mem, value);
            iterator.next();
        }
        assert_eq!(self.count, old_count);
    }
}

impl RememberedSetIterator {
    pub unsafe fn init(remembered_set: &RememberedSet) -> RememberedSetIterator {
        let mut iterator = RememberedSetIterator {
            hash_table: remembered_set.hash_table,
            hash_index: 0,
            use_current_entry: true,
            current_collision: NULL_OBJECT_ID,
        };
        iterator.skip_free();
        iterator
    }

    unsafe fn skip_free(&mut self) {
        let hash_table = self.hash_table.as_blob_mut();
        let length = table_length(hash_table);
        if self.hash_index == length {
            return;
        }
        if self.use_current_entry {
            let entry = table_get(hash_table, self.hash_index);
            if !is_null_value((*entry).value) {
                return;
            }
            debug_assert!(is_null_value((*entry).next_collision));
        } else if !is_null_value(self.current_collision) {
            return;
        }
        self.use_current_entry = true;
        self.hash_index += 1;
        while self.hash_index < length
            && is_null_value((*table_get(hash_table, self.hash_index)).value)
        {
            debug_assert!(is_null_value(
                (*table_get(hash_table, self.hash_index)).next_collision
            ));
            self.hash_index += 1
        }
    }

    pub unsafe fn has_next(&self) -> bool {
        let hash_table = self.hash_table.as_blob_mut();
        self.hash_index < table_length(hash_table)
    }

    pub unsafe fn current(&self) -> Value {
        let entry = self.current_entry();
        (*entry).value
    }

    unsafe fn current_entry(&self) -> *mut HashEntry {
        debug_assert!(self.has_next());
        let hash_table = self.hash_table.as_blob_mut();
        let entry = if self.use_current_entry {
            table_get(hash_table, self.hash_index)
        } else {
            debug_assert!(!is_null_value(self.current_collision));
            let node = as_collision(self.current_collision);
            &mut (*node).entry
        };
        debug_assert!(!is_null_value((*entry).value));
        entry
    }

    pub unsafe fn next(&mut self) {
        let entry = self.current_entry();
        self.use_current_entry = false;
        self.current_collision = (*entry).next_collision;
        self.skip_free()
    }
}

unsafe fn new_table<M: Memory>(mem: &mut M, size: u32) -> Value {
    let object = alloc_blob_internal(mem, Bytes(size * size_of::<HashEntry>() as u32));
    let table = object.as_blob_mut();
    for index in 0..size {
        table_set(table, index, NULL_OBJECT_ID);
    }
    object
}

unsafe fn new_collision_node<M: Memory>(mem: &mut M, value: Value) -> Value {
    debug_assert!(!is_null_value(value));
    let object = alloc_blob_internal(mem, Bytes(size_of::<HashEntry>() as u32));
    let node = as_collision(object);
    (*node).entry = HashEntry {
        value,
        next_collision: NULL_OBJECT_ID,
    };
    object
}

unsafe fn table_get(table: *mut Blob, index: u32) -> *mut HashEntry {
    debug_assert!(table != null_mut());
    let entry =
        (table.payload_addr() as u32 + index * size_of::<HashEntry>() as u32) as *mut HashEntry;
    debug_assert!(
        entry as u32 + size_of::<HashEntry>() as u32
            <= table as u32 + block_size(table as usize).to_bytes().as_u32()
    );
    entry
}

unsafe fn table_set(table: *mut Blob, index: u32, value: Value) {
    let entry = table_get(table, index);
    (*entry).value = value;
    (*entry).next_collision = NULL_OBJECT_ID;
}

unsafe fn table_length(table: *mut Blob) -> u32 {
    debug_assert!(table != null_mut());
    debug_assert!(table.len().as_u32() % size_of::<HashEntry>() as u32 == 0);
    table.len().as_u32() / size_of::<HashEntry>() as u32
}

unsafe fn as_collision(value: Value) -> *mut CollisionNode {
    value.as_blob_mut() as *mut CollisionNode
}

unsafe fn is_null_value(value: Value) -> bool {
    value == NULL_OBJECT_ID
}
