#[cfg(feature = "ic")]
pub mod ic;

use crate::constants::WASM_HEAP_SIZE;
use crate::rts_trap_with;
use crate::types::*;

use motoko_rts_macros::ic_mem_fn;

/// Root set for the garbage collectors.
#[derive(Clone, Copy)]
pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_location: *mut Value,
    // For possible future additional roots, please extend the functionality in:
    // * `gc::copying::copying_gc_internal()`
    // * `gc::mark_compact::mark_compact()`
    // * `gc::generational::GenerationalGC::mark_root_set()`
    // * `gc::generational::GenerationalGC::thread_initial_phase()`
    // * `gc::incremental::roots::visit_roots()`

    // Please note that generational GC requires an additional root set, namely
    // the remembered set to the young generation.
}

/// A trait representing the memory, in particular the heap.
///
/// Offers heap allocation as well as access to the heap limits and the GC root set.
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
    fn get_roots(&self) -> Roots;

    fn get_heap_base(&self) -> usize;
    fn get_last_heap_pointer(&self) -> usize;
    fn get_heap_pointer(&self) -> usize;

    // Used by the GC after a collection run, also sets the last heap pointer.
    unsafe fn shrink_heap(&mut self, new_free_pointer: usize);

    // Only used by object table extension with the incremental GC.
    // Also sets the last heap pointer if it is below the new heap base.
    unsafe fn set_heap_base(&mut self, new_heap_base: usize);

    /// Returns an object address that still needs to be assiged to a new object id to  
    /// obtain a `Value`.
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize;
}

/// Helper for allocating blobs
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, size: Bytes<u32>) -> Value {
    let address = mem.alloc_words(size_of::<Blob>() + size.to_words());
    let object_id = Value::new_object_id(mem, address);

    // NB. Cannot use `as_blob` here as we didn't write the header yet
    let blob = address as *mut Blob;
    (*blob).header.tag = TAG_BLOB;
    (*blob).header.initialize_id(object_id);
    (*blob).len = size;

    object_id
}

/// Helper for allocating arrays
#[ic_mem_fn]
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, len: u32) -> Value {
    // Array payload should not be larger than half of the memory
    if len > (WASM_HEAP_SIZE / 2).0 {
        rts_trap_with("Array allocation too large");
    }

    let address = mem.alloc_words(size_of::<Array>() + Words(len));
    let object_id = Value::new_object_id(mem, address);

    // Cannot use `as_array()` here since the object header is not yet written.
    let ptr: *mut Array = address as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).header.initialize_id(object_id);
    (*ptr).len = len;

    object_id
}
