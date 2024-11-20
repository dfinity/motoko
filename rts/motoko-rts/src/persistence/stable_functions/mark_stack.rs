//! In-heap extendable mark stack for the stable functions collection.
//!
//! Analogous to `gc::incremental::mark_stack`, but with additional 
//! 
//! TODO: Refactor to one code base.
//! 
//! Doubly linked list of stack tables, each containing a series of entries.
//! A table is represented as a blob with the following internal layout:
//!
//! ┌──────────┬─────────┬──────────┬─────────┬──────────────┬────────┐
//! │ previous │   next  | entry[0] |  ...    | entry[top-1] | (free) |
//! └──────────┴─────────┴──────────┴─────────┴──────────────┴────────┘
//!
//! The list is doubly linked for the following purpose:
//! * `previous` to return to the previous table with preceding entries.
//! * `next` avoid repeated allocations when the stack shrinks and regrows.
//!
//! Whenever a table is full and an entry needs to be pushed on the stack,
//! a new stack table is allocated and linked, unless there already exists
//! a next table. Only the last table can have free entry space.
//!
//! NOTES:
//! * The tables are blobs, as their entries do not need to be analyzed by the GC.
//! * The stack tables become garbage after a GC run and can be reclaimed.

use core::ptr::null_mut;

use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Value, TAG_BLOB_B};

pub struct MarkStack {
    last: *mut StackTable,
    top: usize, // index of next free entry in the last stack table
}

pub const STACK_TABLE_CAPACITY: usize = 1018;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct StackEntry {
    pub object: Value,
    pub type_id: u64,
}

#[repr(C)]
struct StackTable {
    pub header: Blob,
    pub previous: *mut StackTable,
    pub next: *mut StackTable,
    pub entries: [StackEntry; STACK_TABLE_CAPACITY],
}

impl MarkStack {
    pub unsafe fn new<M: Memory>(mem: &mut M) -> MarkStack {
        let table = Self::new_table(mem, null_mut());
        MarkStack {
            last: table,
            top: 0,
        }
    }

    pub unsafe fn push<M: Memory>(&mut self, mem: &mut M, value: StackEntry) {
        debug_assert!(self.last != null_mut());
        if self.top == STACK_TABLE_CAPACITY {
            if (*self.last).next == null_mut() {
                self.last = Self::new_table(mem, self.last);
            } else {
                self.last = (*self.last).next;
            }
            self.top = 0;
        }
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        (*self.last).entries[self.top] = value;
        self.top += 1;
        println!(100, "PUSH {}", self.top);
    }

    pub unsafe fn pop(&mut self) -> Option<StackEntry> {
        println!(100, "POP {}", self.top);
        debug_assert!(self.last != null_mut());
        if self.top == 0 {
            if (*self.last).previous == null_mut() {
                return None;
            }
            self.last = (*self.last).previous;
            self.top = STACK_TABLE_CAPACITY;
        }
        debug_assert!(self.top > 0);
        self.top -= 1;
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        Some((*self.last).entries[self.top])
    }

    unsafe fn new_table<M: Memory>(mem: &mut M, previous: *mut StackTable) -> *mut StackTable {
        // No post allocation barrier as this RTS-internal blob will be collected by the GC.
        let table = alloc_blob(mem, TAG_BLOB_B, size_of::<StackTable>().to_bytes()).as_blob_mut()
            as *mut StackTable;
        (*table).previous = previous;
        (*table).next = null_mut();
        if previous != null_mut() {
            (*previous).next = table;
        }
        table
    }
}
