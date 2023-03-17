use std::ptr::null_mut;

use motoko_rts::{
    gc::incremental::{
        object_table::{ObjectTable, OBJECT_TABLE},
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

    let mut mem = TestMemory::new(Words(512 * 1024));

    assert_eq!(OBJECT_TABLE, null_mut());
    assert!(!using_incremental_barrier());
    OBJECT_TABLE = ObjectTable::new(&mut mem, 4);
    create_young_remembered_set(&mut mem);

    let array = alloc_array(&mut mem, 64).as_array();
    test_mark_bit(array as *mut Obj);

    let blob = alloc_blob(&mut mem, Bytes(64)).as_blob_mut();
    test_mark_bit(blob as *mut Obj);

    let stream = alloc_stream(&mut mem, Bytes(64));
    test_mark_bit(stream as *mut Obj);

    take_young_remembered_set();
    OBJECT_TABLE = null_mut();
    assert!(!using_incremental_barrier());
}

unsafe fn test_mark_bit(object: *mut Obj) {
    const TEST_OBJECT_ID: usize = 0;
    let object_id = Value::from_raw(skew(TEST_OBJECT_ID) as u32);
    (*object).initialize_id(object_id);
    assert!(object.object_id() == object_id);
    object.mark();
    assert!(object.is_marked());
    assert!(object.object_id() == object_id);
    object.unmark();
    assert!(!object.is_marked());
    assert!(object.object_id() == object_id);
}
