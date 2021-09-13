#[cfg(feature = "ic")]
pub mod ic;

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
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value;
}

/// Helper for allocating blobs
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, size: Bytes<u32>) -> Value {
    let ptr = mem.alloc_words(size_of::<Blob>() + size.to_words());
    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = ptr.get_ptr() as *mut Blob;
    blob.set_tag();
    blob.set_len(size);
    ptr
}

/// Helper for allocating arrays
#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > 1 << (32 - 2 - 1) {
        // 2 for word size, 1 to divide by two
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = mem.alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.get_ptr() as *mut Array;
    ptr.set_tag();
    ptr.set_len(len);

    skewed_ptr
}
