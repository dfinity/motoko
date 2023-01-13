use motoko_rts::{
    gc::incremental::array_slicing::slice_array,
    memory::alloc_array,
    types::{Obj, Words, TAG_ARRAY, TAG_ARRAY_SLICE_MIN},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing array slicing...");

    let mut mem = TestMemory::new(Words(1024 * 1024));
    test_different_lengths(&mut mem, true);
    test_different_lengths(&mut mem, false);
}

unsafe fn test_different_lengths(mem: &mut TestMemory, marked: bool) {
    // multiple of slice increment
    test_array_slicing(mem, 4096, marked);
    // odd remainder of slice increment
    test_array_slicing(mem, 3999, marked);
    // small array
    test_array_slicing(mem, 10, marked);
    // empty array
    test_array_slicing(mem, 0, marked);
}

unsafe fn test_array_slicing(mem: &mut TestMemory, array_length: u32, marked: bool) {
    let array = alloc_array(mem, array_length).as_array();
    if marked {
        (array as *mut Obj).mark();
    }
    let mut last_offset = 0;
    loop {
        let new_offset = slice_array(array);
        assert!(new_offset > last_offset || array.len() == 0 && new_offset == 0);
        last_offset = new_offset;
        assert_eq!(array.is_marked(), marked);
        if array.tag() == TAG_ARRAY {
            break;
        }
        assert!(array.tag() >= TAG_ARRAY_SLICE_MIN);
    }
    assert_eq!(array.is_marked(), marked);
    assert_eq!(array.len(), array_length);
    assert_eq!(last_offset, array.len());
}
