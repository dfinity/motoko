use std::array::from_fn;

use motoko_rts::gc::incremental::sort::sort;
use proptest::test_runner::{Config, TestRunner};

const TEST_LENGTH: usize = 10_000;

pub fn test() {
    println!("  Testing sort...");

    sort_test(&mut []);
    sort_test(&mut [0]);
    sort_test(&mut [0, 1]);
    sort_test(&mut [1, 0]);
    sort_test(&mut [0, 0]);
    sort_test(&mut from_fn::<_, TEST_LENGTH, _>(|index| index));
    sort_test(&mut from_fn::<_, TEST_LENGTH, _>(|index| {
        TEST_LENGTH - index
    }));

    let strategy = proptest::collection::vec(0..TEST_LENGTH, 0..TEST_LENGTH);
    let mut proptest_runner = TestRunner::new(Config {
        cases: 100,
        failure_persistence: None,
        ..Default::default()
    });
    proptest_runner
        .run(&strategy, |input| {
            let mut array = input;
            sort_test(&mut array);
            Ok(())
        })
        .unwrap();
}

fn sort_test(array: &mut [usize]) {
    unsafe {
        sort(array.as_mut_ptr(), array.len(), &|left, right| {
            left.cmp(&right)
        });
    }
    check_sorted(array);
}

fn check_sorted(array: &[usize]) {
    for index in 1..array.len() {
        assert!(array[index - 1] <= array[index]);
    }
}
