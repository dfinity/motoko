//! Ephemeral used-shape support for the compiler-specialized CandidX encoder.
//! Host allocations are Motoko Blobs, so the ordinary GC reclaims scratch.
//! The caller must not collect, suspend, or retain these pointers after return.
#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), feature(alloc_error_handler))]

extern crate alloc;

mod candid_canonical;

#[cfg(not(test))]
mod host {
    use core::alloc::{GlobalAlloc, Layout};

    #[link(wasm_import_module = "env")]
    unsafe extern "C" {
        fn compute_plugin_alloc(size: usize, align: usize) -> *mut u8;
        pub(super) fn compute_plugin_trap(ptr: *const u8, len: usize) -> !;
    }

    struct Ephemeral;

    unsafe impl GlobalAlloc for Ephemeral {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            unsafe { compute_plugin_alloc(layout.size(), layout.align()) }
        }

        unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
            // Each allocation is already a traced, pointer-free Motoko Blob.
            // Deallocation happens at a later GC increment, after this call.
        }
    }

    #[global_allocator]
    static ALLOCATOR: Ephemeral = Ephemeral;

    #[alloc_error_handler]
    fn allocation_failed(_layout: Layout) -> ! {
        let message = b"CandidX error: scratch allocation failed";
        unsafe { compute_plugin_trap(message.as_ptr(), message.len()) }
    }

    #[panic_handler]
    fn panic(_info: &core::panic::PanicInfo<'_>) -> ! {
        let message = b"CandidX error: runtime invariant failed";
        unsafe { compute_plugin_trap(message.as_ptr(), message.len()) }
    }
}

fn trap_with_prefix(prefix: &str, message: &str) -> ! {
    #[cfg(test)]
    panic!("{prefix}{message}");

    #[cfg(not(test))]
    {
        let mut text = alloc::vec::Vec::with_capacity(prefix.len() + message.len());
        text.extend_from_slice(prefix.as_bytes());
        text.extend_from_slice(message.as_bytes());
        unsafe { host::compute_plugin_trap(text.as_ptr(), text.len()) }
    }
}
