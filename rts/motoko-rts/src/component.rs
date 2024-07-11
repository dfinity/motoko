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

    // TODO: reuse memory space from `cabi_realloc`?
    let value = alloc_blob(mem, Bytes(len));
    let blob = value.as_blob_mut();
    let dest = blob.payload_addr();
    for i in 0..len as usize {
        *dest.add(i) = *items.add(i);
    }
    allocation_barrier(value)
}

/// Canonical ABI allocation logic.
/// Derived from: https://docs.rs/wit-bindgen-rt/0.27.0/src/wit_bindgen_rt/lib.rs.html#54-88
#[ic_mem_fn]
unsafe fn cabi_realloc<M: Memory>(
    _mem: &mut M,
    old_ptr: *mut u8,
    old_len: usize,
    align: usize,
    new_len: usize,
) -> *mut u8 {
    use ::alloc::alloc::{self, Layout};

    let layout;
    let ptr = if old_len == 0 {
        if new_len == 0 {
            return align as *mut u8;
        }
        layout = Layout::from_size_align_unchecked(new_len, align);
        alloc::alloc(layout)
    } else {
        debug_assert_ne!(new_len, 0, "non-zero old_len requires non-zero new_len!");
        layout = Layout::from_size_align_unchecked(old_len, align);
        alloc::realloc(old_ptr, layout, new_len)
    };
    if ptr.is_null() {
        unreachable!();
    }
    ptr
}
