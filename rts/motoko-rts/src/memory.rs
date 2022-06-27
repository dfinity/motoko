#[cfg(feature = "ic")]
pub mod ic;

use crate::constants::WASM_HEAP_SIZE;
use crate::rts_trap_with;
use crate::types::*;

use motoko_rts_macros::ic_mem_fn;

/// A trait for heap allocation. RTS functions allocate in heap via this trait.
///
/// To be able to link the RTS with moc-generated code, we implement wrappers around allocating
/// functions that pass `ic::IcMemory` for the `Memory` arguments, and export these functions with
/// the expected names for the generated code. For example, for a function like
///
/// ```
/// unsafe fn allocating_function<M: Memory>(mem: &mut M) { ... }
/// ```
///
/// we implement (or generate with a macro)
///
/// ```
/// #[no_mangle]
/// unsafe extern "C" fn export_name() { allocating_function(crate::memory::ic::IcMemory) }
/// ```
///
/// This function does not take any `Memory` arguments can be used by the generated code.
pub trait Memory {
    unsafe fn alloc_words(&mut self, n: Words<u32>, init_word: u32) -> Value;
}

/// Helper for fixed-size objects
#[cfg(feature = "ic")]
struct NWords<const N: usize> {}

#[cfg(feature = "ic")]
impl NWords<3> {
    //#[export_name = "alloc_3words"]
    #[inline(always)]
    unsafe fn alloc<M: Memory>(mem: &mut M, init_word: u32) -> Value {
        mem.alloc_words(Words(3), init_word)
    }
}

// create the wrapper
#[cfg(feature = "ic")]
#[export_name = "alloc_3words"]
unsafe extern "C" fn ic_nwords3_alloc(init_word: u32) -> Value {
    NWords::<3>::alloc(&mut crate::memory::ic::IcMemory, init_word)
}

/// Helper for allocating blobs
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, size: Bytes<u32>) -> Value {
    let ptr = mem.alloc_words(size_of::<Blob>() + size.to_words(), TAG_BLOB);
    let blob = ptr.as_blob_mut();
    (*blob).len = size;
    ptr
}

/// Helper for allocating arrays
#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > (WASM_HEAP_SIZE / 2).0 {
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = mem.alloc_words(size_of::<Array>() + Words(len), TAG_ARRAY);
    let arr = skewed_ptr.as_array();
    (*arr).len = len;

    skewed_ptr
}
