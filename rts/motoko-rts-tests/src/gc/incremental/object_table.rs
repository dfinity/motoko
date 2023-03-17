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
}

const TEST_SIZE: usize = 10_000;

unsafe fn test_allocate() {
    let mut mem = TestMemory::new(Words(TEST_SIZE as u32));
    let object_table = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(object_table, &mut expected_table);
    check_all_entries(object_table, &expected_table);
    free_all_entries(object_table, &expected_table);
}

unsafe fn allocate_entries(object_table: *mut ObjectTable, expected_table: &mut [(Value, usize)]) {
    for count in 0..expected_table.len() {
        let address = count * WORD_SIZE;
        let object_id = object_table.new_object_id(address);
        assert_eq!(object_table.get_object_address(object_id), address);
        expected_table[count] = (object_id, address);
        assert_eq!(object_table.get_object_address(object_id), address);
    }
}

unsafe fn check_all_entries(object_table: *mut ObjectTable, expected_table: &[(Value, usize)]) {
    for (object_id, address) in expected_table.iter() {
        assert_eq!(object_table.get_object_address(*object_id), *address);
    }
}

unsafe fn free_all_entries(object_table: *mut ObjectTable, expected_table: &[(Value, usize)]) {
    for (object_id, _) in expected_table.iter() {
        object_table.free_object_id(*object_id);
    }
}

unsafe fn delete_random_half(
    object_table: *mut ObjectTable,
    expected_table: &mut [(Value, usize)],
) -> usize {
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    let mut deleted = 0;
    for index in 0..expected_table.len() {
        if random.rand_u32() % 2 == 0 {
            let object_id = expected_table[index].0;
            object_table.free_object_id(object_id);
            expected_table[index].0 = NULL_OBJECT_ID;
            deleted += 1;
        }
    }
    deleted
}

unsafe fn reallocate(object_table: *mut ObjectTable, expected_table: &mut [(Value, usize)]) {
    let mut free_index = 0;
    while free_index < expected_table.len() && expected_table[free_index].0 != NULL_OBJECT_ID {
        free_index += 1;
    }
    assert!(free_index < expected_table.len());
    let address = expected_table[free_index].1;
    expected_table[free_index].0 = object_table.new_object_id(address);
}

unsafe fn test_remove_realloc() {
    let mut mem = TestMemory::new(Words(TEST_SIZE as u32));
    let object_table = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(object_table, &mut expected_table);
    check_all_entries(object_table, &expected_table);
    let deleted = delete_random_half(object_table, &mut expected_table);
    for _ in 0..deleted {
        reallocate(object_table, &mut expected_table);
    }
    check_all_entries(object_table, &expected_table);
    free_all_entries(object_table, &expected_table);
}

unsafe fn move_all_objects(object_table: *mut ObjectTable, expected_table: &mut [(Value, usize)]) {
    for index in 0..expected_table.len() {
        let (object_id, old_address) = expected_table[index];
        let new_address = old_address + 3 * WORD_SIZE;
        object_table.move_object(object_id, new_address);
        expected_table[index].1 = new_address;
    }
}

unsafe fn test_move() {
    let mut mem = TestMemory::new(Words(TEST_SIZE as u32));
    let object_table = ObjectTable::new(&mut mem, TEST_SIZE);
    let mut expected_table = [(NULL_OBJECT_ID, 0); TEST_SIZE];
    allocate_entries(object_table, &mut expected_table);
    check_all_entries(object_table, &expected_table);
    move_all_objects(object_table, &mut expected_table);
    check_all_entries(object_table, &expected_table);
}
