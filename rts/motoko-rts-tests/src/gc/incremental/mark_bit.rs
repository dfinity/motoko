use motoko_rts::{
    gc::incremental::object_table::ObjectTable,
    memory::{alloc_array, alloc_blob, Memory},
    stream::alloc_stream,
    types::{skew, Bytes, Obj, Value, Words, OBJECT_TABLE},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing mark bit...");

    let mut mem = TestMemory::new(Words(256));

    OBJECT_TABLE = Some(create_object_table(&mut mem, 3));

    let array = alloc_array(&mut mem, 64).as_array();
    test_mark_bit(array as *mut Obj);

    let blob = alloc_blob(&mut mem, Bytes(64)).as_blob_mut();
    test_mark_bit(blob as *mut Obj);

    let stream = alloc_stream(&mut mem, Bytes(64));
    test_mark_bit(stream as *mut Obj);

    OBJECT_TABLE = None;
}

fn create_object_table(mem: &mut TestMemory, length: usize) -> ObjectTable {
    let size = Words(length as u32);
    let base = unsafe { mem.alloc_words(size) } as *mut usize;
    ObjectTable::new(base, length)
}

unsafe fn test_mark_bit(object: *mut Obj) {
    const ARBITRARY_ID: usize = 0x1234560;
    let object_id = Value::from_raw(skew(ARBITRARY_ID) as u32);
    (*object).initialize_id(object_id);
    assert!(object.object_id() == object_id);
    object.mark();
    assert!(object.is_marked());
    assert!(object.object_id() == object_id);
    object.unmark();
    assert!(!object.is_marked());
    assert!(object.object_id() == object_id);
}
