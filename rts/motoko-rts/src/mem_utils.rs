use crate::types::{Bytes, Words};

pub(crate) unsafe fn memcpy_words(to: usize, from: usize, n: Words<u32>) {
    libc::memcpy(to as *mut _, from as *const _, n.to_bytes().0 as usize);
}

pub(crate) unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<u32>) {
    libc::memcpy(to as *mut _, from as *const _, n.0 as usize);
}

pub(crate) unsafe fn memzero(to: usize, n: Bytes<u32>) {
    libc::memset(to as *mut _, 0, n.as_usize());
}
