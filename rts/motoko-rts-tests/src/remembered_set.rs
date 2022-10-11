use motoko_rts::gc::experimental::remembered_set::{RememberedSet, MAX_ENTRIES_PER_TABLE};
use motoko_rts::types::{Words, Value};
use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("Testing mark stack ...");

    test_insert_iterate(0);
    test_insert_iterate(1);
    test_insert_iterate(MAX_ENTRIES_PER_TABLE - 1);
    test_insert_iterate(MAX_ENTRIES_PER_TABLE);
    test_insert_iterate(MAX_ENTRIES_PER_TABLE + 1);
    test_insert_iterate(2 * MAX_ENTRIES_PER_TABLE - 1);
    test_insert_iterate(2 * MAX_ENTRIES_PER_TABLE);
    test_insert_iterate(2 * MAX_ENTRIES_PER_TABLE + 1);
    test_insert_iterate(128 * MAX_ENTRIES_PER_TABLE);
}

unsafe fn test_insert_iterate(amount: u32) {
    println!("  Testing insert/iterate {amount}");

    let mut mem = TestMemory::new(Words(2 * amount + 1024 * 1024));

    let mut remembered_set = RememberedSet::new(&mut mem);
    for value in 0..amount {
        remembered_set.insert(Value::from_scalar(value));
    }

    let mut iterator = remembered_set.iterate();
    for expected in 0..amount {
        assert!(iterator.has_next());
        let actual = iterator.current().get_scalar();
        assert_eq!(actual, expected);
        iterator.next();
    }
    assert!(!iterator.has_next())
}
