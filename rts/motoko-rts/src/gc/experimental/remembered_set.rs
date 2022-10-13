//! Remembered set.
//! Serves for recording pointer locations trapped by the write barrier.
//! The remembered set may store duplicates, as it is optimized for fast insertion.
//!
//! Linked list of record tables, each containing a series of entries.
//! A table is represented as a blob with the following internal layout:
//!
//! ┌──────────┬───────┬─────────┬──────┬────────────────┬────────┐
//! │ next_ptr │ count │ entry[0]|  ... | entry[count-1] | (free) |
//! └──────────┴───────┴─────────┴──────┴────────────────┴────────┘
//!
//! `next_ptr` points to the next record table (unless null)
//! `count` is the number of stored entries in the table.
//! New entries are stored at the beginning of the free table space.
//! Otherwise, if the table is a full, a new next table is appended.
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
use crate::types::{Blob, Bytes, Value};

pub struct RememberedSet {
    first: *mut Blob,
    last: *mut Blob,
    size: usize,
    cache: Value, // optimization, least recently inserted value
}

pub struct RememberedSetIterator {
    table: *mut Blob,
    index: u32,
}

const TABLE_LENGTH: u32 = 1024 - size_of::<Blob>() as u32;
const TABLE_BLOB_SIZE: u32 = TABLE_LENGTH * WORD_SIZE;
const NEXT_POINTER_OFFSET: u32 = 0;
const COUNT_ENTRIES_OFFSET: u32 = 1;
const FIRST_ENTRY_OFFSET: u32 = 2;
pub const MAX_ENTRIES_PER_TABLE: u32 = TABLE_LENGTH - FIRST_ENTRY_OFFSET;

impl RememberedSet {
    pub unsafe fn new<M: Memory>(mem: &mut M) -> RememberedSet {
        let table = Self::new_table(mem);
        RememberedSet {
            first: table,
            last: table,
            size: 0,
            cache: Self::null_ptr(),
        }
    }

    pub unsafe fn insert<M: Memory>(&mut self, mem: &mut M, value: Value) {
        if value.is_null_ptr() || self.cache.get_raw() == value.get_raw() {
            return;
        }
        self.cache = value;
        //println!(100, "Remembered set insert: {:#x}", value.get_raw());
        let mut table = self.last;
        let mut count = table_get(table, COUNT_ENTRIES_OFFSET).get_scalar();
        if count == MAX_ENTRIES_PER_TABLE {
            self.append_table(mem);
            table = self.last;
            debug_assert_eq!(table_get(table, COUNT_ENTRIES_OFFSET).get_scalar(), 0);
            count = 0;
        }
        debug_assert!(count < MAX_ENTRIES_PER_TABLE);
        table_set(table, FIRST_ENTRY_OFFSET + count, value);
        table_set(table, COUNT_ENTRIES_OFFSET, Value::from_scalar(count + 1));
        self.size += 1;
    }

    unsafe fn append_table<M: Memory>(&mut self, mem: &mut M) {
        debug_assert_eq!(
            table_get(self.last, COUNT_ENTRIES_OFFSET).get_scalar(),
            MAX_ENTRIES_PER_TABLE
        );
        let next = Self::new_table(mem);
        debug_assert!(table_get(self.last, NEXT_POINTER_OFFSET).is_null_ptr());
        table_set(
            self.last,
            NEXT_POINTER_OFFSET,
            Value::from_ptr(next as usize),
        );
        self.last = next;
    }

    unsafe fn new_table<M: Memory>(mem: &mut M) -> *mut Blob {
        let table = alloc_blob(mem, Bytes(TABLE_BLOB_SIZE)).as_blob_mut();
        table_set(table, NEXT_POINTER_OFFSET, Self::null_ptr());
        table_set(table, COUNT_ENTRIES_OFFSET, Value::from_scalar(0));
        table
    }

    pub fn iterate(&self) -> RememberedSetIterator {
        RememberedSetIterator {
            table: self.first,
            index: 0,
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    unsafe fn null_ptr() -> Value {
        Value::from_raw((null_mut() as *mut usize) as u32)
    }
}

impl RememberedSetIterator {
    pub unsafe fn has_next(&self) -> bool {
        debug_assert!(
            self.index < MAX_ENTRIES_PER_TABLE
                || self.index == MAX_ENTRIES_PER_TABLE
                    && table_get(self.table, NEXT_POINTER_OFFSET).is_null_ptr()
        );
        self.index < table_get(self.table, COUNT_ENTRIES_OFFSET).get_scalar()
    }

    pub unsafe fn current(&self) -> Value {
        debug_assert!(self.has_next());
        table_get(self.table, FIRST_ENTRY_OFFSET + self.index)
    }

    pub unsafe fn next(&mut self) {
        debug_assert!(self.has_next());
        self.index += 1;
        if self.index == MAX_ENTRIES_PER_TABLE {
            let next = table_get(self.table, NEXT_POINTER_OFFSET);
            if !next.is_null_ptr() {
                self.table = next.as_blob_mut();
                self.index = 0;
            }
        }
    }
}

unsafe fn table_get(table: *mut Blob, index: u32) -> Value {
    debug_assert!(index < TABLE_LENGTH);
    let address = (table.payload_addr() as u32 + index * WORD_SIZE) as *mut u32;
    Value::from_raw(*address)
}

unsafe fn table_set(table: *mut Blob, index: u32, value: Value) {
    debug_assert!(index < TABLE_LENGTH);
    let address = (table.payload_addr() as u32 + index * WORD_SIZE) as *mut u32;
    *address = value.get_raw();
}
