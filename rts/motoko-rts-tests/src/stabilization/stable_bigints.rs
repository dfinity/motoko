use std::ptr::null_mut;

use motoko_rts::{
    bigint::{bigint_add, bigint_eq, bigint_mul, bigint_neg, bigint_of_word64},
    types::Value,
};
use oorandom::Rand32;

use crate::{
    bigint::set_bigint_heap,
    memory::{initialize_test_memory, reset_test_memory},
    stabilization::{deserialize, serialize, stable_memory::clear_stable_memory},
};

pub unsafe fn test() {
    println!("  Testing stable big integers ...");

    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);

    test_simple_numbers();
    test_small_random_numbers(&mut random);
    test_big_random_numbers(&mut random);

    clear_stable_memory();
}

unsafe fn test_simple_numbers() {
    println!("    Testing simple numbers ...");

    for number in 0..256 {
        test_bigint(|| bigint_of_word64(number));
    }
}

unsafe fn test_small_random_numbers(random: &mut Rand32) {
    println!("    Testing small random numbers ...");

    const TEST_RUNS: u32 = 1000;
    for _ in 0..TEST_RUNS {
        let number = random.rand_u32() as u64;
        test_bigint(|| bigint_of_word64(number));
    }
}

unsafe fn test_big_random_numbers(random: &mut Rand32) {
    println!("    Testing big random numbers ...");

    const TEST_RUNS: u32 = 10_000;
    for _ in 0..TEST_RUNS {
        test_bigint(|| random_bigint(random));
    }
}

unsafe fn random_bigint(random: &mut Rand32) -> Value {
    const STEPS: usize = 20;
    let mut last = bigint_of_word64(random.rand_u32() as u64);
    let mut current = bigint_of_word64(random.rand_u32() as u64);
    for _ in 0..STEPS {
        let computed = match random.rand_range(0..3) {
            0 => bigint_add(last, current),
            1 => bigint_mul(last, current),
            2 => bigint_neg(current),
            _ => unreachable!(),
        };
        last = current;
        current = computed;
    }
    current
}

unsafe fn test_bigint<F: FnMut() -> Value>(mut generate_bigint: F) {
    let mut memory = initialize_test_memory();
    set_bigint_heap(&mut memory);
    let input = generate_bigint();
    // Clone the input bigint object, because it is destructed on serialization.
    let clone = bigint_add(input, bigint_of_word64(0));
    assert!(bigint_eq(clone, input));
    let stable_size = serialize(clone, 0);
    // Note: `clone` is no longer a valid bigint because it has been replaced by a forwarding object.
    let output = deserialize(&mut memory, 0, stable_size);
    assert!(bigint_eq(output, input));
    set_bigint_heap(null_mut());
    reset_test_memory();
}
