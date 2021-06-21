use motoko_rts::heap::Heap;
use motoko_rts::types::{Bytes, SkewedPtr, Words, WORD_SIZE};

pub struct TestHeap {
    heap: Box<[u8]>,
}

impl TestHeap {
    pub fn new(size: Words<u32>) -> TestHeap {
        todo!()
    }
}

impl Heap for TestHeap {
    unsafe fn get_heap_base(&mut self) -> u32 {
        todo!()
    }

    unsafe fn get_hp(&mut self) -> u32 {
        todo!()
    }

    unsafe fn set_hp(&mut self, hp: u32) {
        todo!()
    }

    unsafe fn get_static_roots(&mut self) -> SkewedPtr {
        todo!()
    }

    unsafe fn get_closure_table_loc(&mut self) -> *mut SkewedPtr {
        todo!()
    }

    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        todo!()
    }

    unsafe fn grow_memory(&mut self, ptr: usize) {
        todo!()
    }

    unsafe fn note_live_size(&mut self, live_size: Bytes<u32>) {
        todo!()
    }

    unsafe fn note_reclaimed(&mut self, reclaimed: Bytes<u32>) {
        todo!()
    }
}
