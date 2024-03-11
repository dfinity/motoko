use motoko_rts::memory::Memory;
use motoko_rts::types::{Value, Words};
use motoko_rts_macros::*;

pub struct TestMemory {
    heap: Box<[u8]>,
    hp: usize,
}

impl TestMemory {
    pub fn new(size: Words<u32>) -> TestMemory {
        let bytes = size.to_bytes().as_usize();
        let heap = vec![0u8; bytes].into_boxed_slice();
        let hp = heap.as_ptr() as usize;
        TestMemory { heap, hp }
    }

    #[incremental_gc]
    pub fn heap_base(&self) -> usize {
        self.heap.as_ptr() as usize
    }

    #[incremental_gc]
    pub fn heap_end(&self) -> usize {
        self.heap_base() + self.heap.len()
    }

    #[incremental_gc]
    pub fn heap_pointer(&self) -> usize {
        self.hp
    }

    #[incremental_gc]
    pub fn set_heap_pointer(&mut self, heap_pointer: usize) {
        assert!(heap_pointer >= self.heap_base());
        assert!(heap_pointer <= self.heap_end());
        self.hp = heap_pointer;
    }
}

impl Memory for TestMemory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.hp;
        let new_hp = old_hp + bytes.as_usize();
        self.hp = new_hp;

        // Grow memory if needed
        self.grow_memory(new_hp as u64);

        Value::from_ptr(old_hp)
    }

    unsafe fn grow_memory(&mut self, ptr: u64) {
        let heap_end = self.heap.as_ptr() as usize + self.heap.len();
        if ptr as usize > heap_end {
            // We don't allow growing memory in tests, allocate large enough for the test
            panic!(
                "TestMemory::grow_memory called: heap_end={:#x}, grow_memory argument={:#x}",
                heap_end, ptr
            );
        }
    }
}
