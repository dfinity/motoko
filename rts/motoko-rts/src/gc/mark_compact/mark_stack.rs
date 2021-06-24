//! A stack for marking heap objects (for GC). There should be no allocation after the stack
//! otherwise things will break as we push. This invariant is checked in debug builds.

use crate::heap::Heap;
use crate::types::{Blob, Words};

use core::ptr::null_mut;

/// Initial stack size
pub const INIT_STACK_SIZE: Words<u32> = Words(64);

/// Pointer to the `blob` object for the mark stack. Used to get the capacity of the stack.
static mut STACK_BLOB_PTR: *mut Blob = null_mut();

/// Bottom of the mark stack
static mut STACK_BASE: *mut usize = null_mut();

/// Top of the mark stack
static mut STACK_TOP: *mut usize = null_mut();

/// Next free slot in the mark stack
static mut STACK_PTR: *mut usize = null_mut();

pub unsafe fn alloc_mark_stack<H: Heap>(heap: &mut H) {
    debug_assert!(STACK_BLOB_PTR.is_null());

    // Allocating an actual object here to not break dump_heap
    STACK_BLOB_PTR = heap.alloc_blob(INIT_STACK_SIZE.to_bytes()).unskew() as *mut Blob;
    STACK_BASE = STACK_BLOB_PTR.payload_addr() as *mut usize;
    STACK_PTR = STACK_BASE;
    STACK_TOP = STACK_BASE.add(INIT_STACK_SIZE.0 as usize);
}

pub unsafe fn free_mark_stack() {
    STACK_BLOB_PTR = null_mut();
    STACK_BASE = null_mut();
    STACK_PTR = null_mut();
    STACK_TOP = null_mut();
}

/// Doubles the stack size
unsafe fn grow_stack<H: Heap>(heap: &mut H) {
    let stack_cap: Words<u32> = STACK_BLOB_PTR.len().to_words();
    let p = heap.alloc_words(stack_cap).unskew() as *mut usize;

    // Make sure nothing was allocated after the stack
    debug_assert_eq!(STACK_TOP, p);

    let new_cap: Words<u32> = Words(stack_cap.0 * 2);
    (*STACK_BLOB_PTR).len = new_cap.to_bytes();
}

pub unsafe fn push_mark_stack<H: Heap>(heap: &mut H, obj: usize) {
    if STACK_PTR == STACK_TOP {
        grow_stack(heap);
    }

    *STACK_PTR = obj;
    STACK_PTR = STACK_PTR.add(1);
}

pub unsafe fn pop_mark_stack() -> Option<usize> {
    if STACK_PTR == STACK_BASE {
        None
    } else {
        STACK_PTR = STACK_PTR.sub(1);
        Some(*STACK_PTR)
    }
}
