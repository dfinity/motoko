#[cfg(feature = "ic")]
pub mod ic;

use crate::constants::MAX_ARRAY_SIZE;
use crate::rts_trap_with;
use crate::types::*;

#[cfg(feature = "ic")]
use crate::constants::MB;

use motoko_rts_macros::ic_mem_fn;

// Memory reserve in bytes ensured during update and initialization calls.
// For use by queries and upgrade calls.
#[cfg(feature = "ic")]
pub(crate) const GENERAL_MEMORY_RESERVE: usize = 256 * MB;

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
    // General allocator working for all GC variants.
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value;

    // Grow the allocated memory size to at least the address of `ptr`.
    unsafe fn grow_memory(&mut self, ptr: u64);
}

/// Allocate a new blob.
/// Note: After initialization, the post allocation barrier needs to be applied to all mutator objects.
/// For RTS-internal blobs that can be collected by the next GC run, the post allocation barrier can be omitted.
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, tag: u32, size: Bytes<u32>) -> Value {
    let ptr = mem.alloc_words(size_of::<Blob>() + size.to_words());
    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = ptr.get_ptr() as *mut Blob;
    (*blob).header.tag = tag;
    (*blob).header.init_forward(ptr);
    (*blob).len = size;

    ptr
}

/// Allocate a new array.
/// Note: After initialization, the post allocation barrier needs to be applied to all mutator objects.
#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, tag: u32, len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > MAX_ARRAY_SIZE {
        rts_trap_with("Array allocation too large");
    }

    let skewed_ptr = mem.alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.get_ptr() as *mut Array;
    (*ptr).header.tag = tag;
    (*ptr).header.init_forward(skewed_ptr);
    (*ptr).len = len;

    skewed_ptr
}
