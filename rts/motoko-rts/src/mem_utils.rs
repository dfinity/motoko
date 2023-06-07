use crate::types::{Bytes, Words};
use core::ffi::c_void;

type size_t = usize;
type c_int = i32;

extern "C" {
    fn memcpy(dest: *mut c_void, src: *const c_void, n: size_t) -> *mut c_void;
    fn memset(dest: *mut c_void, c: c_int, n: size_t) -> *mut c_void;
}

pub(crate) unsafe fn memcpy_words(to: usize, from: usize, n: Words<u32>) {
    memcpy(to as *mut _, from as *const _, n.to_bytes().as_usize());
}

pub(crate) unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<u32>) {
    memcpy(to as *mut _, from as *const _, n.as_usize());
}

pub(crate) unsafe fn memzero(to: usize, n: Words<u32>) {
    memset(to as *mut _, 0, n.to_bytes().as_usize());
}
