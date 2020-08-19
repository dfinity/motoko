use crate::types::*;

// Provided by rts/test_rts.c
extern "C" {
    pub(crate) fn alloc_bytes(n: Bytes<u32>) -> SkewedPtr;
    pub(crate) fn alloc_words(n: Words<u32>) -> SkewedPtr;
}
