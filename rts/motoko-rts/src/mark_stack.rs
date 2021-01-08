//! A stack for marking heap objects (for GC). There should be no allocation after the stack
//! otherwise things will break as we push.

use crate::alloc::{alloc_blob, alloc_words};
use crate::types::{size_of, Blob, Words};

/// Initial stack size
const INIT_STACK_SIZE: Words<u32> = Words(64);

/// Current stack
static mut STACK_PTR: *mut Blob = core::ptr::null_mut();

/// Current length
static mut STACK_LEN: Words<u32> = Words(0);

pub unsafe fn alloc_mark_stack() {
    debug_assert!(STACK_PTR.is_null());

    // Allocating an actual object here to not break dump_heap
    STACK_PTR = alloc_blob(INIT_STACK_SIZE.to_bytes()).unskew() as *mut Blob;
    STACK_LEN = Words(0);
}

pub unsafe fn free_mark_stack() {
    STACK_PTR = core::ptr::null_mut();
    STACK_LEN = Words(0);
}

/// Doubles the stack size
unsafe fn grow_stack() {
    let stack_cap: Words<u32> = STACK_PTR.len().to_words();
    let new_cap: Words<u32> = Words(stack_cap.0 * 2);
    let p = alloc_words(new_cap).unskew() as *mut usize;

    // Make sure nothing was allocated after the stack
    assert_eq!((size_of::<Blob>() + STACK_PTR.len().to_words()).0, p as u32);

    (*STACK_PTR).len = new_cap.to_bytes();
}

pub unsafe fn push_mark_stack(obj: usize) {
    if STACK_LEN == (*STACK_PTR).len.to_words() {
        grow_stack();
    }

    *(STACK_PTR.payload_addr() as *mut usize).add(STACK_LEN.0 as usize) = obj;
    STACK_LEN += Words(1);
}

pub unsafe fn pop_mark_stack() -> Option<usize> {
    if STACK_LEN.0 == 0 {
        None
    } else {
        STACK_LEN -= Words(1);
        let p = *(STACK_PTR.payload_addr() as *mut usize).add(STACK_LEN.0 as usize);
        Some(p)
    }
}
