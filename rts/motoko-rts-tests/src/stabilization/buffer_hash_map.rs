use std::mem::size_of;

use motoko_rts::{
    constants::WORD_SIZE,
    stabilization::buffered_hash_map::BufferedHashMap,
    types::{Blob, Words},
};
use oorandom::Rand32;

use crate::memory::TestMemory;

const BUFFER_SIZE: Words<u32> = Words((4 * 1024 * 1024 + size_of::<Blob>() as u32) / WORD_SIZE);

pub unsafe fn test() {
    println!("  Testing buffered hash map ...");
    test_empty_hash_map();
    test_single_entry();
    test_multiple_entries();
    test_randomized_entries(1000);
    test_randomized_entries(10_000);
    test_randomized_entries(100_000);
}

fn test_empty_hash_map() {
    println!("    Testing empty hash map ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut hash_map = BufferedHashMap::<usize, u64>::new(&mut memory, 0);
    hash_map.free();
}

fn test_single_entry() {
    println!("    Testing single entry ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut hash_map = BufferedHashMap::<usize, u64>::new(&mut memory, 0);
    hash_map.add(123, 456);
    assert_eq!(hash_map.get(123), 456);
    hash_map.free();
}

fn test_multiple_entries() {
    println!("    Testing multiple entries ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut hash_map = BufferedHashMap::<usize, u64>::new(&mut memory, 0);
    const AMOUNT: usize = 10_000;
    for number in 0..AMOUNT {
        hash_map.add(number, (number * number) as u64);
    }
    for number in 0..AMOUNT {
        assert_eq!(hash_map.get(number), (number * number) as u64);
    }
    hash_map.free();
}

fn test_randomized_entries(amount: usize) {
    println!("    Testing {amount} randomized entries ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut hash_map =
        BufferedHashMap::<u64, usize>::new(&mut memory, random.rand_range(0..1024) as u64);
    let mut numbers = vec![];
    for _ in 0..amount {
        let key = random.rand_u32() as u64 * random.rand_u32() as u64;
        let value = random.rand_u32() as usize;
        numbers.push((key, value));
        hash_map.add(key, value);
    }
    for (key, value) in numbers {
        assert_eq!(hash_map.get(key), value);
    }
    hash_map.free();
}
