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
//! The entries represent ids (references) ßto objects to be visited by the GC.
//! (being conceptually gray in the incremental tri-color mark scheme).
//!
//! NOTES:
//! * The tables are blobs, as their entries must not be visited by the GC.
//! * The entries on the stack must be object ids (Value) as their objects may
//!   move even during mark phase due to object table extension.
//! * The mark stack must use object ids for referencing the previous/next
//!   tables, because the tables could be moved due to object table extension.
//!   If the table of the old generation are moved to the young generation, they
//!   will be recorded in the remembered set such that they will be promoted back
//!   to the old generation.
//! * The stack tables become garbage after a GC run and can be reclaimed.
//! * The GC reserves object ids before GC increment start such that the mark
//!   stack allocation does not trigger an object table growth during a GC
//!   increment.

use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Obj, Value, NULL_OBJECT_ID};

pub struct MarkStack {
    last: Value,
    top: usize, // Index of next free entry in the last stack table.
}

pub const STACK_TABLE_CAPACITY: usize = 1018;

#[repr(C)]
struct StackTable {
    pub header: Blob,
    pub previous: Value,
    pub next: Value,
    pub entries: [Value; STACK_TABLE_CAPACITY],
}

impl MarkStack {
    /// Create an empty mark stack that still needs to be allocated before use.
    /// To avoid slow `Option<MarkStack>`, the stack is allocated and freed by
    /// separate functions.
    pub const fn new() -> MarkStack {
        MarkStack {
            last: NULL_OBJECT_ID,
            top: 0,
        }
    }

    /// Allocate the mark stack before use.
    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M) {
        debug_assert!(!self.is_allocated());
        self.last = Self::new_table(mem, NULL_OBJECT_ID);
        debug_assert_eq!(self.top, 0);
    }

    /// Release the mark stack after use.
    pub unsafe fn free(&mut self) {
        #[cfg(debug_assertions)]
        self.assert_is_garbage();

        debug_assert!(self.is_allocated());
        debug_assert!(self.is_empty());
        debug_assert_eq!(self.top, 0);
        self.last = NULL_OBJECT_ID
        // Stack and their object ids are freed by the GC.
    }

    pub fn is_allocated(&self) -> bool {
        self.last != NULL_OBJECT_ID
    }

    /// Push an object address on the stack.
    pub unsafe fn push<M: Memory>(&mut self, mem: &mut M, object: Value) {
        debug_assert!(object != NULL_OBJECT_ID);
        debug_assert!(self.is_allocated());
        let mut table = self.last.as_blob_mut() as *mut StackTable;
        if self.top == STACK_TABLE_CAPACITY {
            if (*table).next == NULL_OBJECT_ID {
                self.last = Self::new_table(mem, self.last);
            } else {
                self.last = (*table).next;
            }
            table = self.last.as_blob_mut() as *mut StackTable;
            self.top = 0;
        }
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        (*table).entries[self.top] = object;
        self.top += 1;
    }

    /// Pop an object address off the stack, if it is not empty.
    /// Otherwise, if empty, returns `NULL_OBJECT_ID`.
    pub unsafe fn pop(&mut self) -> Value {
        debug_assert!(self.is_allocated());
        let mut table = self.last.as_blob_mut() as *mut StackTable;
        if self.top == 0 {
            if (*table).previous == NULL_OBJECT_ID {
                return NULL_OBJECT_ID;
            }
            self.last = (*table).previous;
            table = self.last.as_blob_mut() as *mut StackTable;
            self.top = STACK_TABLE_CAPACITY;
        }
        debug_assert!(self.top > 0);
        self.top -= 1;
        debug_assert!(self.top < STACK_TABLE_CAPACITY);
        let object = (*table).entries[self.top];
        debug_assert!(object != NULL_OBJECT_ID);
        object
    }

    /// Determine whether the stack is empty.
    pub unsafe fn is_empty(&self) -> bool {
        debug_assert!(self.is_allocated());
        let table = self.last.as_blob_mut() as *mut StackTable;
        self.top == 0 && (*table).previous == NULL_OBJECT_ID
    }

    unsafe fn new_table<M: Memory>(mem: &mut M, previous: Value) -> Value {
        let table_id = alloc_blob(mem, size_of::<StackTable>().to_bytes());
        let table = table_id.as_blob_mut() as *mut StackTable;
        // No mark bit is set as the blob is to be reclaimeed by the current GC run.
        debug_assert!(!(table as *mut Obj).is_marked());
        (*table).previous = previous;
        (*table).next = NULL_OBJECT_ID;
        if previous != NULL_OBJECT_ID {
            let previous_table = previous.as_blob_mut() as *mut StackTable;
            (*previous_table).next = table_id;
        }
        table_id
    }

    #[cfg(debug_assertions)]
    unsafe fn assert_is_garbage(&self) {
        assert!(self.is_allocated());
        let mut current = self.last;
        let mut table = current.as_blob_mut() as *mut StackTable;
        while (*table).previous != NULL_OBJECT_ID {
            current = (*table).previous;
            table = current.as_blob_mut() as *mut StackTable;
        }
        while current != NULL_OBJECT_ID {
            table = current.as_blob_mut() as *mut StackTable;
            assert!(!(table as *mut Obj).is_marked());
            current = (*table).next;
        }
    }
}
