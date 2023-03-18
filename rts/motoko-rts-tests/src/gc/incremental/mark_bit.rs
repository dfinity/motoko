use std::ptr::null_mut;

use motoko_rts::{
    gc::incremental::{
        object_table::{ObjectTable, OBJECT_TABLE, OBJECT_TABLE_ID},
        write_barrier::{
            create_young_remembered_set, take_young_remembered_set, using_incremental_barrier,
        },
    },
    memory::{alloc_array, alloc_blob},
    stream::alloc_stream,
    types::{skew, Bytes, Obj, Value, Words},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing mark bit...");

    test_with_id(Value::from_raw(skew(2) as u32));
    test_with_id(OBJECT_TABLE_ID);
}

pub unsafe fn test_with_id(test_id: Value) {
    let mut mem = TestMemory::new(Words(512 * 1024));

    assert_eq!(OBJECT_TABLE, null_mut());
    assert!(!using_incremental_barrier());
    OBJECT_TABLE = ObjectTable::new(&mut mem, 4);
    create_young_remembered_set(&mut mem);

    let array = alloc_array(&mut mem, 64).as_array();
    test_mark_bit(array as *mut Obj, test_id);

    let blob = alloc_blob(&mut mem, Bytes(64)).as_blob_mut();
    test_mark_bit(blob as *mut Obj, test_id);

    let stream = alloc_stream(&mut mem, Bytes(64));
    test_mark_bit(stream as *mut Obj, test_id);

    take_young_remembered_set();
    OBJECT_TABLE = null_mut();
    assert!(!using_incremental_barrier());
}

unsafe fn test_mark_bit(object: *mut Obj, test_id: Value) {
    (*object).initialize_id(test_id);
    assert!(object.object_id() == test_id);
    object.mark();
    assert!(object.is_marked());
    assert!(object.object_id() == test_id);
    object.unmark();
    assert!(!object.is_marked());
    assert!(object.object_id() == test_id);
}
