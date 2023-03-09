//! In-heap extendable mark stack for the incremental GC.
//!
//! The mark stack cannot grow contiguously as new objects can be allocated
//! during the GC run and thus during the mark phase. This is why the stack
//! is represented as multiple tables.
//!
//! Doubly linked list of stack tables, each containing a series of entries.
//! A table is represented as a blob with the following internal layout:
//!
//! ┌──────────┬─────────┬──────────┬─────────┬──────────────┬────────┐
//! │ previous │   next  | entry[0] |  ...    | entry[top-1] | (free) |
//! └──────────┴─────────┴──────────┴─────────┴──────────────┴────────┘
//!
//! The list is doubly linked for the following purpose (wihout indirection over
//! the object table):
//! * `previous` to return to the previous table with preceding entries.
//! * `next` avoid repeated allocations when the stack shrinks and regrows.
//!
//! Whenever a table is full and an entry needs to be pushed on the stack,
//! a new stack table is allocated and linked, unless there already exists
//! a next table. Only the last table can have free entry space.
//!
//! The entries represent pointers to objects to be visited by the GC.
//! (being conceptually gray in the incremental tri-color mark scheme).
//!
//! NOTES:
//! * The tables are blobs, as their entries must not be analyzed by the GC.
//! * The mark stack does not use object ids for referencing the previous/next
//!   tables, because the tables are discarded before the compaction phase and
//!   does never move.
//! * The stack tables become garbage after a GC run and can be reclaimed.

use core::ptr::null_mut;

use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Obj};

pub struct MarkStack {
    last: *mut StackTable,
    top: usize, // Index of next free entry in the last stack table.
}

pub const STACK_TABLE_CAPACITY: usize = 1018;

#[repr(C)]
struct StackTable {
    pub header: Blob,
    pub previous: *mut StackTable,
    pub next: *mut StackTable,
    pub entries: [*mut Obj; STACK_TABLE_CAPACITY],
}

impl MarkStack {
    /// Create an empty mark stack that still needs to be allocated before use.
    /// To avoid slow `Option<MarkStack>`, the stack is allocated and freed by
    /// separate functions.
    pub const fn new() -> MarkStack {
        MarkStack {
            last: null_mut(),
            top: 0,
        }
    }

    /// Allocate the mark stack before use.
    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M) {
        debug_assert!(!self.is_allocated());
        self.last = Self::new_table(mem, null_mut());
        debug_assert_eq!(self.top, 0);
    }

    /// Release the mark stack after use.
    pub unsafe fn free(&mut self) {
        #[cfg(debug_assertions)]
        self.assert_is_garbage();

        debug_assert!(self.is_allocated());
        debug_assert!(self.is_empty());
        debug_assert_eq!(self.top, 0);
        self.last = null_mut();
        // Stack and their object ids are freed by the GC.
    }

    fn is_allocated(&self) -> bool {
        self.last != null_mut()
    }

    /// Push an object address on the stack.
    pub unsafe fn push<M: Memory>(&mut self, mem: &mut M, object: *mut Obj) {
        debug_assert_ne!(object, null_mut());
        debug_assert!(self.is_allocated());
        if self.top == STACK_TABLE_CAPACITY {
            if (*self.last).next == null_mut() {
                self.last = Self::new_table(mem, self.last);
            } else {
                self.last = (*self.last).next;
            }
            self.top = 0;
        }
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        (*self.last).entries[self.top] = object;
        self.top += 1;
    }

    /// Pop an object address off the stack, if it is not empty.
    /// Otherwise, if empty, returns `null_mut()`.
    pub unsafe fn pop(&mut self) -> *mut Obj {
        debug_assert!(self.is_allocated());
        if self.top == 0 {
            if (*self.last).previous == null_mut() {
                return null_mut();
            }
            self.last = (*self.last).previous;
            self.top = STACK_TABLE_CAPACITY;
        }
        debug_assert!(self.top > 0);
        self.top -= 1;
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        let object = (*self.last).entries[self.top];
        debug_assert_ne!(object, null_mut());
        object
    }

    /// Determine whether the stack is empty.
    pub unsafe fn is_empty(&self) -> bool {
        debug_assert!(self.is_allocated());
        self.top == 0 && (*self.last).previous == null_mut()
    }

    unsafe fn new_table<M: Memory>(mem: &mut M, previous: *mut StackTable) -> *mut StackTable {
        let table =
            alloc_blob(mem, size_of::<StackTable>().to_bytes()).as_blob_mut() as *mut StackTable;
        // No mark bit is set as the blob is to be reclaimeed by the current GC run.
        debug_assert!(!(table as *mut Obj).is_marked());
        (*table).previous = previous;
        (*table).next = null_mut();
        if previous != null_mut() {
            (*previous).next = table;
        }
        table
    }

    #[cfg(debug_assertions)]
    unsafe fn assert_is_garbage(&self) {
        let mut current = self.last;
        while (*current).previous != null_mut() {
            current = (*current).previous;
        }
        while current != null_mut() {
            assert!(!(current as *mut Obj).is_marked());
            current = (*current).next;
        }
    }
}
