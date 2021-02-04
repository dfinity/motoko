use crate::types::{skew, SkewedPtr, Words};

#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> SkewedPtr {
    skew(libc::malloc(n.to_bytes().0 as usize) as usize)
}
