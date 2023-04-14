use motoko_rts::{
    gc::incremental::array_slicing::slice_array,
    memory::alloc_array,
    types::{Words, TAG_ARRAY, TAG_ARRAY_SLICE_MIN},
};

use crate::memory::{set_memory, TestMemory};

pub unsafe fn test() {
    println!("  Testing array slicing...");

    set_memory(TestMemory::new(Words(1024 * 1024)));
    // multiple of slice increment
    test_array_slicing(4096);
    // odd remainder of slice increment
    test_array_slicing(3999);
    // small array
    test_array_slicing(10);
    // empty array
    test_array_slicing(0);
}

unsafe fn test_array_slicing(array_length: u32) {
    let array = alloc_array(array_length).as_array();
    let mut last_offset = 0;
    loop {
        let new_offset = slice_array(array);
        assert!(new_offset > last_offset || array.len() == 0 && new_offset == 0);
        last_offset = new_offset;
        if (*array).header.tag == TAG_ARRAY {
            break;
        }
        assert!((*array).header.tag >= TAG_ARRAY_SLICE_MIN);
    }
    assert_eq!(array.len(), array_length);
    assert_eq!(last_offset, array.len());
}
