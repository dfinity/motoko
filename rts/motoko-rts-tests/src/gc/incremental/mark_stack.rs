use std::ptr::null_mut;

use crate::{gc::utils::WORD_SIZE, memory::TestMemory};
use motoko_rts::{
    gc::incremental::{
        mark_stack::{MarkStack, STACK_TABLE_CAPACITY},
        object_table::{ObjectTable, OBJECT_TABLE},
    },
    types::{skew, Value, Words},
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
    let mut mem = TestMemory::new(Words(64 * 1024 * 1024));
    debug_assert_eq!(OBJECT_TABLE, null_mut());
    OBJECT_TABLE = ObjectTable::new(&mut mem, 16);

    stack.allocate(&mut mem, false);
    test_internal_push_pop(&mut mem, &mut stack, amount, regrow_step);
    stack.free();
    OBJECT_TABLE = null_mut();
}

unsafe fn test_internal_push_pop(
    mem: &mut TestMemory,
    stack: &mut MarkStack,
    amount: usize,
    regrow_step: usize,
) {
    for count in 0..amount {
        stack.push(mem, synthetic_object_id(count), false);
        if count == regrow_step {
            test_internal_push_pop(mem, stack, amount - count, regrow_step);
        }
    }
    for count in (0..amount).rev() {
        assert!(!stack.is_empty());
        assert!(stack.pop() == synthetic_object_id(count));
    }
}

unsafe fn synthetic_object_id(count: usize) -> Value {
    Value::from_raw(skew(count * WORD_SIZE) as u32)
}
