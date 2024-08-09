// Declarations adopted from https://github.com/rust-lang/libc/blob/main/src/wasi.rs.
#![allow(non_camel_case_types)]

use motoko_rts_macros::classical_persistence;
use motoko_rts_macros::enhanced_orthogonal_persistence;

pub(crate) type c_void = core::ffi::c_void;
pub(crate) type size_t = usize;
pub(crate) type c_char = i8;
pub(crate) type c_int = i32;

#[classical_persistence]
#[cfg(feature = "ic")]
pub(crate) type c_double = f64;

#[classical_persistence]
pub(crate) unsafe fn memcpy(dest: *mut c_void, src: *const c_void, n: size_t) -> *mut c_void {
    libc::memcpy(dest, src, n)
}

#[classical_persistence]
pub(crate) unsafe fn memset(dest: *mut c_void, c: c_int, n: size_t) -> *mut c_void {
    libc::memset(dest, c, n)
}

#[classical_persistence]
pub(crate) unsafe fn memcmp(cx: *const c_void, ct: *const c_void, n: size_t) -> c_int {
    libc::memcmp(cx, ct, n)
}

#[enhanced_orthogonal_persistence]
extern "C" {
    pub(crate) fn memcpy(dest: *mut c_void, src: *const c_void, n: size_t) -> *mut c_void;
    pub(crate) fn memset(dest: *mut c_void, c: c_int, n: size_t) -> *mut c_void;
    pub(crate) fn memcmp(cx: *const c_void, ct: *const c_void, n: size_t) -> c_int;
}
