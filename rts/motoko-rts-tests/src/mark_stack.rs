use crate::page_alloc::TestPageAlloc;

use motoko_rts::gc::mark_compact::mark_stack::MarkStack;
use motoko_rts::page_alloc::PageAlloc;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing mark stack ...");

    test_push_pop();
}

fn test_push_pop() {
    println!("  Testing push/pop");

    let mut proptest_runner = TestRunner::new(Config {
        cases: 100,
        failure_persistence: None,
        ..Default::default()
    });

    proptest_runner
        .run(&(0u32..1000u32), |n_objs| {
            // We test with sizes 1 KiB 1 KiB - one word to test handling unusable one-word space
            // at the end of a mark stack page
            let mut page_alloc = TestPageAlloc::new(1024); // 1 KiB
            if let Err(err) = test_(&mut page_alloc, n_objs) {
                return Err(err);
            }

            let mut page_alloc = TestPageAlloc::new(1020);
            test_(&mut page_alloc, n_objs)
        })
        .unwrap();
}

static TAGS: [Tag; 14] = [
    TAG_OBJECT,
    TAG_OBJ_IND,
    TAG_ARRAY,
    TAG_BITS64,
    TAG_MUTBOX,
    TAG_CLOSURE,
    TAG_SOME,
    TAG_VARIANT,
    TAG_BLOB,
    TAG_FWD_PTR,
    TAG_BITS32,
    TAG_BIGINT,
    TAG_CONCAT,
    TAG_NULL,
];

fn test_<P: PageAlloc>(page_alloc: &mut P, n_objs: u32) -> TestCaseResult {
    let objs: Vec<u32> = (0..n_objs).collect();

    unsafe {
        let mut mark_stack = MarkStack::new(page_alloc.clone());

        for obj in &objs {
            // Pushing a dummy argument derived from `obj` for tag
            mark_stack.push(*obj as usize, TAGS[(*obj as usize) % TAGS.len()]);
        }

        for obj in objs.iter().copied().rev() {
            let popped = mark_stack.pop();
            if popped != Some((obj as usize, TAGS[(obj as usize) % TAGS.len()])) {
                mark_stack.free();
                return Err(TestCaseError::Fail(
                    format!(
                        "Unexpected object popped, expected={:?}, popped={:?}",
                        obj, popped
                    )
                    .into(),
                ));
            }
        }

        mark_stack.free();
    }

    Ok(())
}
