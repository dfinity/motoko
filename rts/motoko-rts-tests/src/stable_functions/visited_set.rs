use std::collections::HashSet;

use crate::memory::TestMemory;
use motoko_rts::gc::functions::visited_set::{
    VisitedSet, VisitedValue, INITIAL_TABLE_LENGTH, OCCUPATION_THRESHOLD_PERCENT,
};
use motoko_rts::types::{Value, Words};

const GROW_LIMIT: usize = INITIAL_TABLE_LENGTH * OCCUPATION_THRESHOLD_PERCENT / 100;

pub unsafe fn test() {
    println!("  Testing visited set ...");

    test_visited_set(0);
    test_visited_set(1);
    test_visited_set(INITIAL_TABLE_LENGTH / 2);
    test_visited_set(GROW_LIMIT - 1);
    test_visited_set(GROW_LIMIT);
    test_visited_set(GROW_LIMIT + 1);
    test_visited_set(INITIAL_TABLE_LENGTH);
    test_visited_set(2 * GROW_LIMIT - 1);
    test_visited_set(2 * GROW_LIMIT);
    test_visited_set(2 * GROW_LIMIT + 1);
    test_visited_set(32 * GROW_LIMIT);
}

unsafe fn test_visited_set(amount: usize) {
    println!("TESTING {amount}");
    test_insert_iterate(amount);
    test_duplicates(amount);
    test_collisions(amount);
}

unsafe fn test_insert_iterate(amount: usize) {
    let mut mem = TestMemory::new(Words(4 * amount + 1024 * 1024));

    let mut visited_set = VisitedSet::new(&mut mem);
    let mut test_set: HashSet<usize> = HashSet::new();
    for value in 0..amount {
        visited_set.insert(&mut mem, test_value(value));
        test_set.insert(value);
    }

    let mut iterator = visited_set.iterate();
    for _ in 0..amount {
        assert!(iterator.has_next());
        let value = iterator.current();
        assert!(test_set.contains(&raw_value(value)));
        iterator.next();
    }
    assert!(!iterator.has_next());
}

unsafe fn test_duplicates(amount: usize) {
    let mut mem = TestMemory::new(Words(4 * amount + 1024 * 1024));

    let mut visited_set = VisitedSet::new(&mut mem);
    for value in 0..amount {
        visited_set.insert(&mut mem, test_value(value));
    }

    for value in 0..amount {
        visited_set.insert(&mut mem, test_value(value));
    }
}

unsafe fn test_collisions(amount: usize) {
    let mut mem = TestMemory::new(Words(4 * amount + 1024 * 1024));

    let mut visited_set = VisitedSet::new(&mut mem);
    let mut test_set: HashSet<usize> = HashSet::new();

    for index in 0..amount {
        const FACTOR: usize = 1024 * 1024;
        let value = if index <= usize::MAX / FACTOR {
            index * FACTOR
        } else {
            index
        };
        visited_set.insert(&mut mem, test_value(value));
        test_set.insert(value);
    }

    let mut iterator = visited_set.iterate();
    for _ in 0..amount {
        assert!(iterator.has_next());
        let value = iterator.current();
        assert!(test_set.contains(&raw_value(value)));
        iterator.next();
    }
    assert!(!iterator.has_next());
}

fn test_value(number: usize) -> VisitedValue {
    VisitedValue {
        object: Value::from_raw(number),
        type_id: number as u64,
    }
}

fn raw_value(value: VisitedValue) -> usize {
    value.object.get_raw()
}
