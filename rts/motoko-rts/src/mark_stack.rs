//! A stack for marking heap objects (for GC). There should be no allocation after the stack
//! otherwise things will break as we push.

use crate::alloc::alloc_words;
use crate::types::Words;

/// Initial stack size
const INIT_STACK_SIZE: usize = 1024;

/// Current stack
/// This is public for testing purposes
static mut STACK_PTR: *mut usize = core::ptr::null_mut();

/// Current capacity
static mut STACK_CAP: usize = 0;

/// Current length
static mut STACK_LEN: usize = 0;

pub unsafe fn alloc_mark_stack() {
    debug_assert!(STACK_PTR.is_null());

    STACK_PTR = alloc_words(Words(INIT_STACK_SIZE as u32)).unskew() as *mut usize;
    STACK_CAP = INIT_STACK_SIZE;
    STACK_LEN = 0;
}

pub unsafe fn free_mark_stack() {
    STACK_PTR = core::ptr::null_mut();
    STACK_CAP = 0;
    STACK_LEN = 0;
}

/// Doubles the stack size
unsafe fn grow_stack() {
    let p = alloc_words(Words(STACK_CAP as u32)).unskew() as *mut usize;
    // Make sure nothing was allocated after the stack
    assert_eq!(STACK_PTR.add(STACK_CAP), p);
    STACK_CAP += STACK_CAP;
}

pub unsafe fn push_mark_stack(obj: usize) {
    if STACK_LEN == STACK_CAP {
        grow_stack();
    }

    *STACK_PTR.add(STACK_LEN) = obj;
    STACK_LEN += 1;
}

pub unsafe fn pop_mark_stack() -> Option<usize> {
    if STACK_LEN == 0 {
        None
    } else {
        STACK_LEN -= 1;
        let p = *STACK_PTR.add(STACK_LEN);
        Some(p)
    }
}
