use motoko_rts::mark_stack::{alloc_mark_stack, free_mark_stack, pop_mark_stack, push_mark_stack};

use quickcheck::{quickcheck, TestResult};

use std::cmp::min;

pub unsafe fn test() {
    println!("Testing mark stack ...");
    quickcheck(test_ as fn(Vec<usize>) -> TestResult);
}

fn test_(objs: Vec<usize>) -> TestResult {
    // We can't test grow_stack as it requires the new allocation to be next to the old allocation,
    // so cap the limit to 1024
    let objs = &objs[0..min(objs.len(), 1024)];

    unsafe {
        // Leaks, but OK
        alloc_mark_stack();

        for obj in objs {
            push_mark_stack(*obj);
        }

        for obj in objs.iter().rev() {
            let popped = pop_mark_stack();
            if popped != Some(*obj) {
                free_mark_stack(); // TODO: Does not really free
                return TestResult::error(format!(
                    "Unexpected object popped, expected={:?}, popped={:?}",
                    obj, popped
                ));
            }
        }

        free_mark_stack(); // TODO: Does not really free
    }

    TestResult::passed()
}
