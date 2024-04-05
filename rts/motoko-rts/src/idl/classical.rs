use super::idl_sub_internal;

#[no_mangle]
unsafe extern "C" fn idl_sub(
    rel_buf: *mut usize, // a buffer with at least 2 * typtbl_size1 * typtbl_size2 bits
    typtbl1: *mut *mut u8,
    typtbl2: *mut *mut u8,
    typtbl_end1: *mut u8,
    typtbl_end2: *mut u8,
    typtbl_size1: usize,
    typtbl_size2: usize,
    t1: i32,
    t2: i32,
) -> bool {
    idl_sub_internal(
        rel_buf,
        typtbl1,
        typtbl2,
        typtbl_end1,
        typtbl_end2,
        typtbl_size1,
        typtbl_size2,
        t1,
        t2,
    )
}
