// Custom RTS function utilities

use alloc::vec::Vec;
use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    types::{Bytes, Value, TAG_BLOB},
};

unsafe fn blob_fn(
    mem: &mut impl Memory,
    blob: Value,
    function: impl FnOnce(Vec<u8>) -> Vec<u8>,
) -> Value {
    if blob.tag() != TAG_BLOB {
        panic!("expected Blob");
    }
    let blob = blob.as_blob();
    let len = blob.len().as_u32(); // `usize` here?
    let vec = (0..len).into_iter().map(|i| blob.get(i)).collect();
    let result = function(vec);
    let result_value = alloc_blob(mem, Bytes(result.len() as u32));
    let blob = result_value.as_blob_mut();
    let mut dest = blob.payload_addr();

    for i in 0..result.len() {
        *dest = result[i];
        dest = dest.add(1);
    }

    allocation_barrier(result_value)
}

// Temporary example
#[no_mangle]
pub unsafe extern "C" fn echo(value: u32) -> u32 {
    value
}

// Temporary example
#[ic_mem_fn]
unsafe fn modify_blob<M: Memory>(mem: &mut M, value: Value) -> Value {
    blob_fn(mem, value, |mut vec| {
        vec.push('!' as u8);
        vec
    })
}

// [external-codegen]
