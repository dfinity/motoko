use motoko_rts::{
    memory::{alloc_array, alloc_blob},
    stream::alloc_stream,
    types::{Bytes, Obj, Words},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("Testing mark bit...");

    let mut mem = TestMemory::new(Words(256));

    let array = alloc_array(&mut mem, 64).as_array();
    test_mark_bit(array as *mut Obj);

    let blob = alloc_blob(&mut mem, Bytes(64)).as_blob_mut();
    test_mark_bit(blob as *mut Obj);

    let stream = alloc_stream(&mut mem, Bytes(64));
    test_mark_bit(stream as *mut Obj);
}

unsafe fn test_mark_bit(object: *mut Obj) {
    let tag = object.tag();
    object.mark();
    assert!(object.is_marked());
    assert_eq!(object.tag(), tag);
    object.unmark();
    assert!(!object.is_marked());
    assert_eq!(object.tag(), tag);
}
