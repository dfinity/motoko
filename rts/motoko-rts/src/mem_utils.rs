use crate::types::{Bytes, Words};

use crate::libc_declarations::{memcpy, memset};

pub unsafe fn memcpy_words(to: usize, from: usize, n: Words<usize>) {
    memcpy(to as *mut _, from as *const _, n.to_bytes().as_usize());
}

pub unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<usize>) {
    memcpy(to as *mut _, from as *const _, n.as_usize());
}

pub unsafe fn memzero(to: usize, n: Words<usize>) {
    memset(to as *mut _, 0, n.to_bytes().as_usize());
}

#[cfg(feature = "ic")]
pub unsafe fn memzero_bytes(to: usize, n: Bytes<usize>) {
    memset(to as *mut _, 0, n.as_usize());
}
