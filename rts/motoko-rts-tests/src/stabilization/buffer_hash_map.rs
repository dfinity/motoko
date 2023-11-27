use std::mem::size_of;

use motoko_rts::{
    constants::WORD_SIZE,
    stabilization::{
        buffered_hash_map::BufferedHashMap, buffered_stable_memory::BufferedStableMemory,
    },
    types::{Blob, Words},
};
use oorandom::Rand32;

use crate::memory::TestMemory;

const BUFFER_SIZE: Words<u32> = Words((4 * 1024 * 1024 + size_of::<Blob>() as u32) / WORD_SIZE);

pub unsafe fn test() {
    println!("  Testing buffered hash map ...");
    let mut memory = TestMemory::new(BUFFER_SIZE);
    let mut buffer = BufferedStableMemory::open(&mut memory, 0);
    test_empty_hash_map(&mut buffer);
    test_single_entry(&mut buffer);
    test_multiple_entries(&mut buffer);
    test_randomized_entries(&mut buffer, 1000);
    test_randomized_entries(&mut buffer, 10_000);
    test_randomized_entries(&mut buffer, 100_000);
    buffer.close();
}

fn test_empty_hash_map(buffer: &mut BufferedStableMemory) {
    println!("    Testing empty hash map ...");
    let mut hash_map = BufferedHashMap::<usize, u64>::new(buffer);
    assert!(hash_map.get(0).is_none());
}

fn test_single_entry(buffer: &mut BufferedStableMemory) {
    println!("    Testing single entry ...");
    let mut hash_map = BufferedHashMap::<usize, u64>::new(buffer);
    hash_map.add(123, 456);
    assert_eq!(hash_map.get(123).unwrap(), 456);
}

fn test_multiple_entries(buffer: &mut BufferedStableMemory) {
    println!("    Testing multiple entries ...");
    let mut hash_map = BufferedHashMap::<usize, u64>::new(buffer);
    const AMOUNT: usize = 10_000;
    for number in 0..AMOUNT {
        hash_map.add(number, (number * number) as u64);
    }
    for number in 0..AMOUNT {
        assert_eq!(hash_map.get(number).unwrap(), (number * number) as u64);
    }
}

fn test_randomized_entries(buffer: &mut BufferedStableMemory, amount: usize) {
    println!("    Testing {amount} randomized entries ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut hash_map = BufferedHashMap::<u64, usize>::new(buffer);
    let mut numbers = vec![];
    for _ in 0..amount {
        let key = random.rand_u32() as u64 * random.rand_u32() as u64;
        let value = random.rand_u32() as usize;
        numbers.push((key, value));
        hash_map.add(key, value);
    }
    for (key, value) in numbers {
        assert_eq!(hash_map.get(key).unwrap(), value);
    }
}
