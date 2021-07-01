use crate::memory::TestMemory;

use motoko_rts::gc::mark_compact::mark_stack::{
    alloc_mark_stack, free_mark_stack, pop_mark_stack, push_mark_stack,
};
use motoko_rts::memory::Memory;
use motoko_rts::types::Words;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing mark stack ...");

    let mut proptest_runner = TestRunner::new(Config {
        cases: 100,
        failure_persistence: None,
        ..Default::default()
    });

    proptest_runner
        .run(&(0u32..1000u32), |n_objs| {
            let mut mem = TestMemory::new(Words(1024 * 1024));
            test_(&mut mem, n_objs)
        })
        .unwrap();
}

fn test_<M: Memory>(mem: &mut M, n_objs: u32) -> TestCaseResult {
    let objs: Vec<u32> = (0..n_objs).collect();

    unsafe {
        alloc_mark_stack(mem);

        for obj in &objs {
            push_mark_stack(mem, *obj as usize, obj.wrapping_sub(1));
        }

        for obj in objs.iter().copied().rev() {
            let popped = pop_mark_stack();
            if popped != Some((obj as usize, obj.wrapping_sub(1))) {
                free_mark_stack();
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
