//! Implements allocation routines used by the generated code and the GC.

use core::arch::wasm32;

use crate::gc;
use crate::rts_trap_with;
use crate::types::{skew, Bytes, SkewedPtr, Words};

#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> SkewedPtr {
    todo!()
}

/// Page allocation. Ensures that the memory up to the given pointer is allocated.
pub(crate) unsafe fn grow_memory(ptr: usize) {
    todo!()
}
