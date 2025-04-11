use crate::memory::TestMemory;
use motoko_rts::{
    gc::functions::mark_stack::{MarkStack, StackEntry, STACK_TABLE_CAPACITY},
    types::{Value, Words},
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
    let mut mem = TestMemory::new(Words(64 * 1024));
    let mut stack = MarkStack::new(&mut mem);
    internal_push_pop(&mut mem, &mut stack, amount, regrow_step);
    assert!(stack.pop().is_none());
}

unsafe fn internal_push_pop(
    mem: &mut TestMemory,
    stack: &mut MarkStack,
    amount: usize,
    regrow_step: usize,
) {
    for count in 0..amount {
        stack.push(mem, test_value(count));
        if count == regrow_step {
            internal_push_pop(mem, stack, amount - count, regrow_step);
        }
    }
    for count in (0..amount).rev() {
        assert_eq!(stack.pop().unwrap(), test_value(count));
    }
}

fn test_value(number: usize) -> StackEntry {
    StackEntry {
        object: Value::from_raw(number),
        type_id: number as u64,
    }
}
