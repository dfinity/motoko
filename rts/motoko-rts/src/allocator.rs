// c.f. https://os.phil-opp.com/heap-allocation/#dynamic-memory

use alloc::alloc::{GlobalAlloc, Layout};
//use core::ptr::null_mut;
use crate::memory::{alloc_blob, ic};
use crate::types::Bytes;

pub struct HeapAllocator;

unsafe impl GlobalAlloc for HeapAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();
        // align is a power of 2
        debug_assert!(align.count_ones() == 1);
        if align <= 4 {
            // payload_addr() is always word-aligned (so also nibble and byte aligned)
            let blob =
                alloc_blob::<ic::IcMemory>(&mut ic::IcMemory, Bytes(size as u32)).as_blob_mut();
            let address = blob.payload_addr();
            debug_assert!(address as usize % align == 0);
            address
        } else {
            // for other alignments (all powers of 2), we need to round up the size and
            // round up payload_addr to nearest aligned address
            let size_up = size + (align - 1);
            let blob =
                alloc_blob::<ic::IcMemory>(&mut ic::IcMemory, Bytes(size_up as u32)).as_blob_mut();
            let payload_addr = blob.payload_addr() as usize;
            let address = (payload_addr + (align - 1)) & (!(align - 1));
            debug_assert!(address % align == 0);
            debug_assert!(address + size - 1 <= payload_addr + size_up - 1);
            address as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // leave to GC
    }
}

#[global_allocator]
static ALLOCATOR: HeapAllocator = HeapAllocator;
