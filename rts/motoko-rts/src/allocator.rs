// c.f. https://os.phil-opp.com/heap-allocation/#dynamic-memory

use alloc::alloc::{GlobalAlloc, Layout};
//use core::ptr::null_mut;
use crate::memory::{alloc_blob, ic};
use crate::types::Bytes;

pub struct HeapAllocator;

unsafe impl GlobalAlloc for HeapAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if !(layout.align() == 4 || layout.align() == 2 || layout.align() == 1) {
            panic!("unsupported allocator alignment");
        };
        let blob = alloc_blob::<ic::IcMemory>(&mut ic::IcMemory, Bytes(layout.size() as u32))
            .as_blob_mut();
        blob.payload_addr()
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // leave to GC
    }
}

#[global_allocator]
static ALLOCATOR: HeapAllocator = HeapAllocator;
