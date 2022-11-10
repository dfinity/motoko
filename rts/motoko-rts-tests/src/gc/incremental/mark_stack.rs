use crate::memory::TestMemory;
use motoko_rts::{
    gc::incremental::mark_stack::{MarkStack, STACK_TABLE_CAPACITY},
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
    for count in 0..amount {
        stack.push(&mut mem, Value::from_scalar(count as u32));
        if count == regrow_step {
            test_push_pop(amount - count, regrow_step);
        }
    }
    for count in (0..amount).rev() {
        assert_eq!(stack.pop().unwrap().get_scalar() as usize, count);
    }
    assert!(stack.pop().is_none());
}
