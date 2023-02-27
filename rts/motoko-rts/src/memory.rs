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
/// Returns an object address that still needs to be assiged to a new object id to  
/// obtain a `Value`.
pub trait Memory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize;
}

/// Helper for allocating blobs
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, size: Bytes<u32>) -> Value {
    let address = mem.alloc_words(size_of::<Blob>() + size.to_words());
    let id = Value::new_object_id(address);

    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = address as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).header.id = id;
    (*blob).len = size;

    id
}

/// Helper for allocating arrays
#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > (WASM_HEAP_SIZE / 2).0 {
        rts_trap_with("Array allocation too large");
    }

    let address = mem.alloc_words(size_of::<Array>() + Words(len));
    let id = Value::new_object_id(address);

    // Cannot use `as_array()` here since the object header is not yet written.
    let ptr: *mut Array = address as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).header.id = id;
    (*ptr).len = len;

    id
}
