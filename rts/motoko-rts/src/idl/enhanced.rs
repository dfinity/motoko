use super::idl_sub_internal;
use crate::{memory::Memory, types::Value};
use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn]
unsafe fn idl_sub<M: Memory>(
    mem: &mut M,
    rel_buf: *mut usize, // a buffer with at least 2 * typtbl_size1 * typtbl_size2 bits
    typtbl1: *mut *mut u8,
    typtbl_end1: *mut u8,
    typtbl_size1: usize,
    candid_data2: Value,
    type_offsets2: Value,
    t1: i32,
    t2: i32,
) -> bool {
    use crate::persistence::compatibility::TypeDescriptor;

    let mut type_descriptor2 = TypeDescriptor::new(candid_data2, type_offsets2);
    let typtbl2 = type_descriptor2.build_type_table(mem);
    let typtbl_end2 = type_descriptor2.type_table_end();
    let typtbl_size2 = type_descriptor2.type_count();

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
