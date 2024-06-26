use crate::types::{Bytes, Words};

// Potentially optimized Wasm bulk memory operation, defined by the compiler backend.
extern "C" {
    fn memory_copy(to: usize, from: usize, n: Bytes<u32>);
    fn memory_fill(to: usize, value: i32, n: Bytes<u32>);
}

pub(crate) unsafe fn memcpy_words(to: usize, from: usize, n: Words<u32>) {
    memory_copy(to, from, n.to_bytes());
}

pub(crate) unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<u32>) {
    memory_copy(to, from, n);
}

pub(crate) unsafe fn memzero(to: usize, n: Words<u32>) {
    memory_fill(to, 0, n.to_bytes());
}

// Legacy call used by `ic-ref` with missing Wasm bulk-memory operation support.
#[no_mangle]
pub unsafe extern "C" fn legacy_memcpy(to: usize, from: usize, n: Bytes<u32>) {
    libc::memcpy(to as *mut _, from as *const _, n.as_usize());
}

// Legacy call used by `ic-ref` with missing Wasm bulk-memory operation support.
#[no_mangle]
pub unsafe extern "C" fn legacy_memset(dest: usize, c: i32, n: Bytes<u32>) {
    libc::memset(dest as *mut _, c, n.as_usize());
}

pub(crate) unsafe fn memzero_bytes(to: usize, n: Bytes<u32>) {
    libc::memset(to as *mut _, 0, n.as_usize());
}
