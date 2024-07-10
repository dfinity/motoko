// Wasm Component Model utilities

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    types::Value,
    Bytes,
};
use alloc::vec::Vec;
use motoko_rts_macros::ic_mem_fn;

/// Convert a Canonical ABI `list<u8>` to a Motoko `Blob`.
#[ic_mem_fn]
unsafe fn blob_of_cabi_list_u8<M: Memory>(mem: &mut M, vec: Vec<u8>) -> Value {
    let value = alloc_blob(mem, Bytes(vec.len() as u32));
    let blob = value.as_blob_mut();
    let mut dest = blob.payload_addr();
    for item in vec.into_iter() {
        *dest = item;
        dest = dest.add(1);
    }
    allocation_barrier(value)
}
