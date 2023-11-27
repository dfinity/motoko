//! In-heap extendable stack used for heap traversal during graph-copy-based
//! stabilization.
//!
//! Similar to incremental GC's mark stack, but, however, with some differences:
//! * The values stored on the stack are generic, and could also be the slicing
//!   information for arrays. No specific sentinel value is used to denote the
//!   end of the stack.
//! * The stack tables are handled as normal objects, with allocation barrier
//!   been called. This is primarily done by principle (same pattern for all
//!   mutator logic), although it would not really be required in the case of
//!   stabilization. The mark stack for the incremental GC is immediately reclaimable,
//!   i.e. no allocation barrier is called.
//! TODO: Conside unifying the stack data structure for common use by both the
//! incremental GC and the stabilization logic.
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
//! Note:
//! * The tables are blobs, as their entries must not be analyzed by the GC.

use core::ptr::null_mut;

use crate::barriers::allocation_barrier;
use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob};

pub struct TraversalStack<T: Copy> {
    last: *mut StackTable<T>,
    top: usize, // index of next free entry in the last stack table
}

pub const STACK_TABLE_CAPACITY: usize = 1018;

#[repr(C)]
struct StackTable<T: Copy> {
    pub header: Blob,
    pub previous: *mut StackTable<T>,
    pub next: *mut StackTable<T>,
    pub entries: [T; STACK_TABLE_CAPACITY],
}

impl<T: Copy> TraversalStack<T> {
    pub fn new<M: Memory>(mem: &mut M) -> TraversalStack<T> {
        let table = unsafe { Self::new_table(mem, null_mut()) };
        TraversalStack {
            last: table,
            top: 0,
        }
    }

    pub fn push<M: Memory>(&mut self, mem: &mut M, value: T) {
        unsafe {
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
        }
    }

    pub fn pop(&mut self) -> T {
        unsafe {
            debug_assert!(self.last != null_mut());
            if self.top == 0 {
                if (*self.last).previous == null_mut() {
                    panic!("Empty stack")
                }
                self.last = (*self.last).previous;
                self.top = STACK_TABLE_CAPACITY;
            }
            debug_assert!(self.top > 0);
            self.top -= 1;
            debug_assert!(self.top < STACK_TABLE_CAPACITY);
            (*self.last).entries[self.top]
        }
    }

    pub fn is_empty(&self) -> bool {
        unsafe {
            debug_assert!(self.last != null_mut());
            self.top == 0 && (*self.last).previous == null_mut()
        }
    }

    unsafe fn new_table<M: Memory>(
        mem: &mut M,
        previous: *mut StackTable<T>,
    ) -> *mut StackTable<T> {
        let blob = alloc_blob(mem, size_of::<StackTable<T>>().to_bytes());
        let table = blob.as_blob_mut() as *mut StackTable<T>;
        (*table).previous = previous;
        (*table).next = null_mut();
        if previous != null_mut() {
            (*previous).next = table;
        }
        allocation_barrier(blob);
        table
    }
}
