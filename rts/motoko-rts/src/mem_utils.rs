use crate::types::{Bytes, Words};

use crate::libc_declarations::{memcpy, memset};

pub(crate) unsafe fn memcpy_words(to: usize, from: usize, n: Words<u32>) {
    memcpy(to as *mut _, from as *const _, n.to_bytes().as_usize());
}

pub(crate) unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<u32>) {
    memcpy(to as *mut _, from as *const _, n.as_usize());
}

pub(crate) unsafe fn memzero(to: usize, n: Words<u32>) {
    memset(to as *mut _, 0, n.to_bytes().as_usize());
}
