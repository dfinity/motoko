use std::ptr::null_mut;

use motoko_rts::{
    gc::incremental::object_table::{ObjectTable, OBJECT_TABLE},
    types::{Value, Words, NULL_OBJECT_ID},
};
use oorandom::Rand32;

use crate::{gc::utils::WORD_SIZE, memory::TestMemory};

pub unsafe fn test() {
    println!("  Testing object table ...");
    assert_eq!(OBJECT_TABLE, null_mut());
    test_allocate();
    test_remove_realloc();
    test_move();
    test_grow();
    assert_eq!(OBJECT_TABLE, null_mut());
}

const TEST_SIZE: usize = 10_000;
const ALLOC_SIZE: Words<u32> = Words(TEST_SIZE as u32 + 6);

unsafe fn test_allocate() {
    let mut mem = TestMemory::new(ALLOC_SIZE);
    OBJECT_TABLE = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(&mut mem, &mut expected_table);
    check_all_entries(&expected_table);
    free_all_entries(&expected_table);
    OBJECT_TABLE = null_mut();
}

unsafe fn allocate_entries(mem: &mut TestMemory, expected_table: &mut [(Value, usize)]) {
    for count in 0..expected_table.len() {
        let address = count * WORD_SIZE;
        let object_id = ObjectTable::new_object_id(mem, address);
        assert_eq!(OBJECT_TABLE.get_object_address(object_id), address);
        expected_table[count] = (object_id, address);
        assert_eq!(OBJECT_TABLE.get_object_address(object_id), address);
    }
}

unsafe fn check_all_entries(expected_table: &[(Value, usize)]) {
    for (object_id, address) in expected_table.iter() {
        assert_eq!(OBJECT_TABLE.get_object_address(*object_id), *address);
    }
}

unsafe fn free_all_entries(expected_table: &[(Value, usize)]) {
    for (object_id, _) in expected_table.iter() {
        OBJECT_TABLE.free_object_id(*object_id);
    }
}

unsafe fn delete_random_half(expected_table: &mut [(Value, usize)]) -> usize {
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut deleted = 0;
    for index in 0..expected_table.len() {
        if random.rand_u32() % 2 == 0 {
            let object_id = expected_table[index].0;
            OBJECT_TABLE.free_object_id(object_id);
            expected_table[index].0 = NULL_OBJECT_ID;
            deleted += 1;
        }
    }
    deleted
}

unsafe fn reallocate(mem: &mut TestMemory, expected_table: &mut [(Value, usize)]) {
    let mut free_index = 0;
    while free_index < expected_table.len() && expected_table[free_index].0 != NULL_OBJECT_ID {
        free_index += 1;
    }
    assert!(free_index < expected_table.len());
    let address = expected_table[free_index].1;
    expected_table[free_index].0 = ObjectTable::new_object_id(mem, address);
}

unsafe fn test_remove_realloc() {
    let mut mem = TestMemory::new(ALLOC_SIZE);
    OBJECT_TABLE = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(&mut mem, &mut expected_table);
    check_all_entries(&expected_table);
    let deleted = delete_random_half(&mut expected_table);
    for _ in 0..deleted {
        reallocate(&mut mem, &mut expected_table);
    }
    check_all_entries(&expected_table);
    free_all_entries(&expected_table);
    OBJECT_TABLE = null_mut();
}

unsafe fn move_all_objects(expected_table: &mut [(Value, usize)]) {
    for index in 0..expected_table.len() {
        let (object_id, old_address) = expected_table[index];
        let new_address = old_address + 3 * WORD_SIZE;
        OBJECT_TABLE.move_object(object_id, new_address);
        expected_table[index].1 = new_address;
    }
}

unsafe fn test_move() {
    let mut mem = TestMemory::new(ALLOC_SIZE);
    OBJECT_TABLE = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(&mut mem, &mut expected_table);
    check_all_entries(&expected_table);
    move_all_objects(&mut expected_table);
    check_all_entries(&expected_table);
    OBJECT_TABLE = null_mut();
}

unsafe fn test_grow() {
    let mut mem = TestMemory::new(Words(ALLOC_SIZE.as_u32() * 3));
    OBJECT_TABLE = ObjectTable::new(&mut mem, 1);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(&mut mem, &mut expected_table);
    check_all_entries(&expected_table);
    free_all_entries(&expected_table);
    OBJECT_TABLE = null_mut();
}
