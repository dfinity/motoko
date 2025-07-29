#[cfg(feature = "ic")]
pub mod ic;
use crate::{constants::MAX_ARRAY_LENGTH_FOR_ITERATOR, types::*};

use motoko_rts_macros::classical_persistence;
use motoko_rts_macros::enhanced_orthogonal_persistence;
use motoko_rts_macros::ic_mem_fn;

#[cfg(feature = "ic")]
use crate::constants::MB;

#[enhanced_orthogonal_persistence]
use crate::constants::GB;

// TODO: Redesign for 64-bit support by using a dynamic partition list.
/// Currently limited to 64 GB.
#[enhanced_orthogonal_persistence]
pub const MAXIMUM_MEMORY_SIZE: Bytes<u64> = Bytes(64u64 * GB as u64);

#[classical_persistence]
pub const MAXIMUM_MEMORY_SIZE: Bytes<u64> = Bytes(usize::MAX as u64);

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
    unsafe fn alloc_words(&mut self, n: Words<usize>) -> Value;

    // Grow the allocated memory size to at least the address of `ptr`.
    unsafe fn grow_memory(&mut self, ptr: usize);
}

/// Allocate a new blob.
/// Note: After initialization, the post allocation barrier needs to be applied to all mutator objects.
/// For RTS-internal blobs that can be collected by the next GC run, the post allocation barrier can be omitted.
#[ic_mem_fn]
pub unsafe fn alloc_blob<M: Memory>(mem: &mut M, tag: Tag, size: Bytes<usize>) -> Value {
    debug_assert!(is_blob_tag(tag));
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
pub unsafe fn alloc_array<M: Memory>(mem: &mut M, tag: Tag, len: usize) -> Value {
    debug_assert!(is_base_array_tag(tag));
    assert!(len <= MAX_ARRAY_LENGTH_FOR_ITERATOR);

    let skewed_ptr = mem.alloc_words(size_of::<Array>() + Words(len));

    let ptr: *mut Array = skewed_ptr.get_ptr() as *mut Array;
    (*ptr).header.tag = tag;
    (*ptr).header.init_forward(skewed_ptr);
    (*ptr).len = len;

    skewed_ptr
}

/// Allocate a new weak reference.
#[enhanced_orthogonal_persistence]
#[ic_mem_fn]
pub unsafe fn alloc_weak_ref<M: Memory>(mem: &mut M, target: Value) -> Value {
    use crate::barriers::allocation_barrier;

    let weak_ref = mem.alloc_words(size_of::<WeakRef>() + Words(16));

    let weak_ref_obj = weak_ref.get_ptr() as *mut WeakRef;
    (*weak_ref_obj).header.tag = TAG_WEAK_REF;
    (*weak_ref_obj).header.init_forward(weak_ref);
    (*weak_ref_obj).field = target;

    // // DEBUG CODE!!!
    // let mut buffer = [0u8; 32];
    // let hex_str = format_dec(weak_ref.get_raw(), "alloc_weak_ref: ", &mut buffer);
    // use crate::rts_trap_with;
    // rts_trap_with(hex_str);

    // TODO: double check this!!!
    allocation_barrier(weak_ref)
}

/// Check if a weak reference is still live.
#[enhanced_orthogonal_persistence]
#[ic_mem_fn]
pub unsafe fn weak_ref_is_live<M: Memory>(mem: &mut M, weak_ref: Value) -> bool {
    // DEBUG CODE!!!
    // use crate::rts_trap_with;
    // let raw_value = weak_ref.get_raw();
    // let mut buffer = [0u8; 32];
    // let hex_str = format_dec(raw_value, "is_live: ", &mut buffer);
    // rts_trap_with(hex_str);

    if !weak_ref.is_non_null_ptr() {
        return false; // Invalid WeakRef pointer.
    }
    let weak_ref_obj = weak_ref.get_ptr() as *mut WeakRef;
    let field_tag = (*weak_ref_obj).header.tag;

    match field_tag {
        TAG_WEAK_REF => {
            return true; // For now, always return true.
        }
        _ => {
            return false;
        }
    }
}

// Helper function to format decimal with custom prefix
fn format_dec<'a>(num: usize, prefix: &str, buffer: &'a mut [u8]) -> &'a str {
    let prefix_bytes = prefix.as_bytes();
    let prefix_len = prefix_bytes.len();

    // Copy prefix to buffer
    for (i, &byte) in prefix_bytes.iter().enumerate() {
        if i < buffer.len() {
            buffer[i] = byte;
        }
    }

    // Handle zero case
    if num == 0 {
        if prefix_len < buffer.len() {
            buffer[prefix_len] = b'0';
        }
        return core::str::from_utf8(&buffer[0..prefix_len + 1]).unwrap();
    }

    // Count digits needed
    let mut temp = num;
    let mut digits = 0;
    while temp > 0 {
        digits += 1;
        temp /= 10;
    }

    // Convert to string (right to left) after the prefix
    temp = num;
    let mut pos = prefix_len + digits;

    while temp > 0 && pos > prefix_len {
        pos -= 1;
        buffer[pos] = b'0' + (temp % 10) as u8;
        temp /= 10;
    }

    core::str::from_utf8(&buffer[0..prefix_len + digits]).unwrap()
}
