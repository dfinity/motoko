use motoko_rts::{
    memory::{alloc_array, alloc_blob},
    stream::alloc_stream,
    types::{is_marked, mark, unmark, Bytes, Obj, Words},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing mark bit...");

    test_mark_functions();

    let mut mem = TestMemory::new(Words(256));

    let array = alloc_array(&mut mem, 64).as_array();
    test_mark_bit(array as *mut Obj);

    let blob = alloc_blob(&mut mem, Bytes(64)).as_blob_mut();
    test_mark_bit(blob as *mut Obj);

    let stream = alloc_stream(&mut mem, Bytes(64));
    test_mark_bit(stream as *mut Obj);
}

fn test_mark_functions() {
    let unmarked_tag = 0x12345678;
    assert!(!is_marked(unmarked_tag));
    let marked_tag = mark(unmarked_tag);
    assert!(is_marked(marked_tag));
    assert_eq!(unmark(marked_tag), unmarked_tag);
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
