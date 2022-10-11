//! Remembered set.
//! Serves for recording pointer locations trapped by the write barrier.
//! The remembered set may store duplicates, as it is optimized for fast insertion.
//!
//! Linked list of record tables, each containing a series of entries.
//! A table is represented as an array with the following internal layout:
//!
//! ┌──────────┬───────┬─────────┬──────┬────────────────┬────────┐
//! │ next_ptr │ count │ entry[0]|  ... | entry[count-1] | (free) |
//! └──────────┴───────┴─────────┴──────┴────────────────┴────────┘
//!
//! `next_ptr` points to the next record table (unless null)
//! `count` is the number of stored entries in the table.
//! New entries are stored at the beginning of the free table space.
//! Otherwise, if the table is a full, a new next table is appended.

use core::mem::size_of;

use crate::memory::{alloc_array, Memory};
use crate::types::{Array, Value, TAG_NULL};

pub struct RememberedSet {
    first: *mut Array,
    last: *mut Array,
    cache: Value, // optimization, least recently inserted value
}

pub struct RememberedSetIterator {
    table: *mut Array,
    index: u32,
}

const TABLE_ARRAY_LENGTH: u32 = 1024 - size_of::<Array>() as u32;
const NEXT_POINTER_OFFSET: u32 = 0;
const COUNT_ENTRIES_OFFSET: u32 = 1;
const FIRST_ENTRY_OFFSET: u32 = 2;
pub const MAX_ENTRIES_PER_TABLE: u32 = TABLE_ARRAY_LENGTH - FIRST_ENTRY_OFFSET;
const NULL_POINTER: Value = Value::from_raw(TAG_NULL);

impl RememberedSet {
    pub unsafe fn new<M: Memory>(mem: &mut M) -> RememberedSet {
        let table = Self::new_table(mem);
        RememberedSet {
            first: table,
            last: table,
            cache: Value::from_raw(TAG_NULL),
        }
    }

    pub unsafe fn insert<M: Memory>(&mut self, mem: &mut M, value: Value) {
        if value.is_null() || self.cache.get_raw() == value.get_raw() {
            return;
        }
        self.cache = value;
        //println!(100, "Remembered set insert: {:#x}", value.get_raw());
        let mut table = self.last;
        let mut count = table.get(COUNT_ENTRIES_OFFSET).get_scalar();
        if count == MAX_ENTRIES_PER_TABLE {
            self.append_table(mem);
            table = self.last;
            debug_assert_eq!(table.get(COUNT_ENTRIES_OFFSET).get_scalar(), 0);
            count = 0;
        }
        debug_assert!(count < MAX_ENTRIES_PER_TABLE);
        table.set(FIRST_ENTRY_OFFSET + count, value);
        table.set(COUNT_ENTRIES_OFFSET, Value::from_scalar(count + 1));
    }

    unsafe fn append_table<M: Memory>(&mut self, mem: &mut M) {
        debug_assert_eq!(
            self.last.get(COUNT_ENTRIES_OFFSET).get_scalar(),
            MAX_ENTRIES_PER_TABLE
        );
        let next = Self::new_table(mem);
        debug_assert_eq!(
            self.last.get(NEXT_POINTER_OFFSET).get_raw(),
            NULL_POINTER.get_raw()
        );
        self.last
            .set(NEXT_POINTER_OFFSET, Value::from_ptr(next as usize));
        self.last = next;
    }

    unsafe fn new_table<M: Memory>(mem: &mut M) -> *mut Array {
        let table = alloc_array(mem, TABLE_ARRAY_LENGTH).as_array();
        table.set(NEXT_POINTER_OFFSET, NULL_POINTER);
        table.set(COUNT_ENTRIES_OFFSET, Value::from_scalar(0));
        table
    }

    pub fn iterate(&self) -> RememberedSetIterator {
        RememberedSetIterator {
            table: self.first,
            index: 0,
        }
    }
}

impl RememberedSetIterator {
    pub unsafe fn has_next(&self) -> bool {
        debug_assert!(
            self.index < MAX_ENTRIES_PER_TABLE
                || self.index == MAX_ENTRIES_PER_TABLE
                    && self.table.get(NEXT_POINTER_OFFSET).is_null()
        );
        self.index < self.table.get(COUNT_ENTRIES_OFFSET).get_scalar()
    }

    pub unsafe fn current(&self) -> Value {
        debug_assert!(self.has_next());
        self.table.get(FIRST_ENTRY_OFFSET + self.index)
    }

    pub unsafe fn next(&mut self) {
        debug_assert!(self.has_next());
        self.index += 1;
        if self.index == MAX_ENTRIES_PER_TABLE {
            let next = self.table.get(NEXT_POINTER_OFFSET);
            if !next.is_null() {
                self.table = next.as_array();
                self.index = 0;
            }
        }
    }
}
