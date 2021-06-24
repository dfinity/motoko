use crate::heap::TestHeap;

use motoko_rts::gc::mark_compact::mark_stack::{
    alloc_mark_stack, free_mark_stack, pop_mark_stack, push_mark_stack,
};
use motoko_rts::heap::Heap;
use motoko_rts::types::Words;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing mark stack ...");

    let mut proptest_runner = TestRunner::new(Config {
        cases: 100,
        failure_persistence: None, // TODO: I don't know what this is about, but it generates a
        // warning in runtime
        ..Default::default()
    });

    proptest_runner
        .run(&(0u32..1000u32), |n_objs| {
            let mut heap = TestHeap::new(Words(1024 * 1024));
            test_(&mut heap, n_objs)
        })
        .unwrap();
}

fn test_<H: Heap>(heap: &mut H, n_objs: u32) -> TestCaseResult {
    let objs: Vec<u32> = (0..n_objs).collect();

    unsafe {
        alloc_mark_stack(heap);

        for obj in &objs {
            push_mark_stack(heap, *obj as usize);
        }

        for obj in objs.iter().rev() {
            let popped = pop_mark_stack();
            if popped != Some(*obj as usize) {
                free_mark_stack(); // TODO: Does not really free
                return Err(TestCaseError::Fail(
                    format!(
                        "Unexpected object popped, expected={:?}, popped={:?}",
                        obj, popped
                    )
                    .into(),
                ));
            }
        }

        free_mark_stack();
    }

    Ok(())
}
