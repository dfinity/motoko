use std::collections::HashSet;

use crate::memory::TestMemory;
use motoko_rts::gc::incremental::object_table::ObjectTable;
use motoko_rts::memory::Memory;
use motoko_rts::remembered_set::{
    RememberedSet, INITIAL_TABLE_LENGTH, OCCUPATION_THRESHOLD_PERCENT,
};
use motoko_rts::types::{Value, Words, OBJECT_TABLE};

const GROW_LIMIT: u32 = INITIAL_TABLE_LENGTH * OCCUPATION_THRESHOLD_PERCENT / 100;

pub unsafe fn test() {
    println!("Testing remembered set ...");

    test_series(None);
    test_series(Some(1024 * 1024));
    test_series(Some(1));
}

unsafe fn test_series(object_table_length: Option<usize>) {
    let mut mem = TestMemory::new(Words(8 * 1024 * 1024));

    assert!(OBJECT_TABLE.is_none());
    if object_table_length.is_some() {
        println!(
            "   with object table size {}...",
            object_table_length.unwrap()
        );
        initialize_object_table(&mut mem, object_table_length.unwrap());
    } else {
        println!("   without object table...");
    }

    test_remembered_set(&mut mem, 0);
    test_remembered_set(&mut mem, 1);
    test_remembered_set(&mut mem, INITIAL_TABLE_LENGTH / 2);
    test_remembered_set(&mut mem, GROW_LIMIT - 1);
    test_remembered_set(&mut mem, GROW_LIMIT);
    test_remembered_set(&mut mem, GROW_LIMIT + 1);
    test_remembered_set(&mut mem, INITIAL_TABLE_LENGTH);
    test_remembered_set(&mut mem, 2 * GROW_LIMIT - 1);
    test_remembered_set(&mut mem, 2 * GROW_LIMIT);
    test_remembered_set(&mut mem, 2 * GROW_LIMIT + 1);
    test_remembered_set(&mut mem, 128 * GROW_LIMIT);

    if object_table_length.is_some() {
        OBJECT_TABLE = None;
    }
}

unsafe fn initialize_object_table(mem: &mut TestMemory, length: usize) {
    let size = Words(length as u32);
    let base = unsafe { mem.alloc_words(size) } as *mut usize;
    let object_table = ObjectTable::new(base, length);
    let new_heap_address = object_table.end();
    OBJECT_TABLE = Some(object_table);
    mem.set_heap_base(new_heap_address);
}

unsafe fn test_remembered_set(mem: &mut TestMemory, amount: u32) {
    test_insert_iterate(mem, amount);
    test_duplicates(mem, amount);
    test_collisions(mem, amount);
}

unsafe fn test_insert_iterate(mem: &mut TestMemory, amount: u32) {
    let mut remembered_set = RememberedSet::new(mem);
    let mut test_set: HashSet<u32> = HashSet::new();
    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for value in 1..amount + 1 {
        remembered_set.insert(mem, Value::from_raw(value));
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

unsafe fn test_duplicates(mem: &mut TestMemory, amount: u32) {
    let mut remembered_set = RememberedSet::new(mem);
    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for value in 1..amount + 1 {
        remembered_set.insert(mem, Value::from_raw(value));
    }

    let count = remembered_set.count();
    for value in 1..amount + 1 {
        remembered_set.insert(mem, Value::from_raw(value));
        assert_eq!(remembered_set.count(), count);
    }
}

unsafe fn test_collisions(mem: &mut TestMemory, amount: u32) {
    let mut remembered_set = RememberedSet::new(mem);
    let mut test_set: HashSet<u32> = HashSet::new();

    // start at 1 since 0 is the null ptr and not stored in the remembered set
    for index in 1..amount + 1 {
        const FACTOR: u32 = 1024 * 1024;
        let value = if index <= u32::MAX / FACTOR {
            index * FACTOR
        } else {
            index
        };
        remembered_set.insert(mem, Value::from_raw(value));
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
