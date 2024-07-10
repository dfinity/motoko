// Wasm Component Model utilities

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    types::Value,
    Bytes,
};
use motoko_rts_macros::ic_mem_fn;

/// Convert a Canonical ABI `list<u8>` pointer to a Motoko `Blob`.
#[ic_mem_fn]
unsafe fn blob_of_cabi<M: Memory>(mem: &mut M, ret: *const u32) -> Value {
    let items = *ret as *const u8; // Note: 32-bit address
    let len = *ret.add(1);

    // TODO: use existing Blob from `cabi_realloc`?
    let value = alloc_blob(mem, Bytes(len));
    let blob = value.as_blob_mut();
    let dest = blob.payload_addr();
    for i in 0..len as usize {
        *dest.add(i) = *items.add(i);
    }
    allocation_barrier(value)
}
