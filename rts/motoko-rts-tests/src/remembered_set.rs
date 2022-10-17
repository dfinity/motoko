use std::collections::HashSet;

use crate::memory::TestMemory;
use motoko_rts::gc::generational::remembered_set::{
    RememberedSet, INITIAL_TABLE_LENGTH, OCCUPATION_THRESHOLD_PERCENT,
};
use motoko_rts::types::{Value, Words};

const GROW_LIMIT: u32 = INITIAL_TABLE_LENGTH * OCCUPATION_THRESHOLD_PERCENT / 100;

pub unsafe fn test() {
    println!("Testing mark stack ...");

    test_remembered_set(0);
    test_remembered_set(1);
    test_remembered_set(INITIAL_TABLE_LENGTH / 2);
    test_remembered_set(GROW_LIMIT - 1);
    test_remembered_set(GROW_LIMIT);
    test_remembered_set(GROW_LIMIT + 1);
    test_remembered_set(INITIAL_TABLE_LENGTH);
    test_remembered_set(2 * GROW_LIMIT - 1);
    test_remembered_set(2 * GROW_LIMIT);
    test_remembered_set(2 * GROW_LIMIT + 1);
    test_remembered_set(128 * GROW_LIMIT);
}

unsafe fn test_remembered_set(amount: u32) {
    test_insert_iterate(amount);
    test_duplicates(amount);
    test_collisions(amount);
}

unsafe fn test_insert_iterate(amount: u32) {
    println!("  Testing insert/iterate {amount}");

    let mut mem = TestMemory::new(Words(2 * amount + 1024 * 1024));

    let mut remembered_set = RememberedSet::new(&mut mem);
    let mut test_set: HashSet<u32> = HashSet::new();
    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for value in 1..amount + 1 {
        remembered_set.insert(&mut mem, Value::from_raw(value));
        test_set.insert(value);
    }

    let mut iterator = remembered_set.iterate();
    for _ in 1..amount + 1 {
        assert!(iterator.has_next());
        let value = iterator.current().get_raw();
        assert!(test_set.contains(&value));
        iterator.next();
    }
    assert!(!iterator.has_next());
}

unsafe fn test_duplicates(amount: u32) {
    println!("  Testing duplicates {amount}");

    let mut mem = TestMemory::new(Words(2 * amount + 1024 * 1024));

    let mut remembered_set = RememberedSet::new(&mut mem);
    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for value in 1..amount + 1 {
        remembered_set.insert(&mut mem, Value::from_raw(value));
    }

    let count = remembered_set.count();
    for value in 1..amount + 1 {
        remembered_set.insert(&mut mem, Value::from_raw(value));
        assert_eq!(remembered_set.count(), count);
    }
}

unsafe fn test_collisions(amount: u32) {
    println!("  Testing collisions {amount}");

    let mut mem = TestMemory::new(Words(2 * amount + 1024 * 1024));

    let mut remembered_set = RememberedSet::new(&mut mem);
    let mut test_set: HashSet<u32> = HashSet::new();

    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for index in 1..amount + 1 {
        const FACTOR: u32 = 1024 * 1024;
        let value = if index <= u32::MAX / FACTOR {
            index * FACTOR
        } else {
            index
        };
        remembered_set.insert(&mut mem, Value::from_raw(value));
        test_set.insert(value);
    }

    let mut iterator = remembered_set.iterate();
    for _ in 1..amount + 1 {
        assert!(iterator.has_next());
        let value = iterator.current().get_raw();
        assert!(test_set.contains(&value));
        iterator.next();
    }
    assert!(!iterator.has_next());
}
