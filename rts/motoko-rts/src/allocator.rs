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
        let word_size = crate::constants::WORD_SIZE as usize;
        let min_align = (align + word_size - 1) / word_size * word_size;
        let blob_size = size + min_align - word_size;
        let blob =
            alloc_blob::<ic::IcMemory>(&mut ic::IcMemory, Bytes(blob_size as u32)).as_blob_mut();
        let payload_address = blob.payload_addr() as usize;
        let aligned_address = (payload_address + min_align - 1) / min_align * min_align;

        debug_assert_eq!(aligned_address % layout.align(), 0);
        debug_assert!(aligned_address + size <= payload_address + blob_size);
        aligned_address as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // leave to GC
    }
}

#[global_allocator]
static ALLOCATOR: HeapAllocator = HeapAllocator;
