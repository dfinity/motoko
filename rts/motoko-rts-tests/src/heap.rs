use motoko_rts::heap::Heap;
use motoko_rts::types::{Bytes, SkewedPtr, Words, WORD_SIZE};

pub struct TestHeap {
    heap: Box<[u8]>,
}

impl TestHeap {
    pub fn new(size: Words<u32>) -> TestHeap {
        let bytes = size.to_bytes().0;
        TestHeap {
            heap: vec![0u8; bytes as usize].into_boxed_slice(),
        }
    }
}

impl Heap for TestHeap {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> SkewedPtr {
        todo!()
    }

    unsafe fn grow_memory(&mut self, ptr: usize) {
        todo!()
    }
}
