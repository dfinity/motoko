use std::ptr::null_mut;

use crate::{gc::utils::WORD_SIZE, memory::TestMemory};
use motoko_rts::{
    gc::incremental::mark_stack::{MarkStack, STACK_TABLE_CAPACITY},
    types::{Obj, Words},
};

pub unsafe fn test() {
    println!("  Testing mark stack ...");

    test_push_pop(0, usize::MAX);
    test_push_pop(1, usize::MAX);
    test_push_pop(2, usize::MAX);
    test_push_pop(STACK_TABLE_CAPACITY - 1, STACK_TABLE_CAPACITY / 4);
    test_push_pop(STACK_TABLE_CAPACITY, STACK_TABLE_CAPACITY / 4);
    test_push_pop(STACK_TABLE_CAPACITY + 1, STACK_TABLE_CAPACITY / 4);
    test_push_pop(2 * STACK_TABLE_CAPACITY - 1, STACK_TABLE_CAPACITY);
    test_push_pop(2 * STACK_TABLE_CAPACITY, STACK_TABLE_CAPACITY);
    test_push_pop(2 * STACK_TABLE_CAPACITY + 1, STACK_TABLE_CAPACITY);
    test_push_pop(10_000, 2500);
}

unsafe fn test_push_pop(amount: usize, regrow_step: usize) {
    let mut stack = MarkStack::new();
    let mut mem = TestMemory::new(Words(64 * 1024));
    stack.allocate(&mut mem);
    for count in 0..amount {
        stack.push(&mut mem, synthetic_object_address(count));
        if count == regrow_step {
            test_push_pop(amount - count, regrow_step);
        }
    }
    for count in (0..amount).rev() {
        assert!(!stack.is_empty());
        assert_eq!(stack.pop(), synthetic_object_address(count));
    }
    assert!(stack.is_empty());
    assert_eq!(stack.pop(), null_mut());
    stack.free();
}

fn synthetic_object_address(count: usize) -> *mut Obj {
    ((count + 1) * WORD_SIZE) as *mut Obj
}
