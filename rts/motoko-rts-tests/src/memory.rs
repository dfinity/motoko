use motoko_rts::memory::Memory;
use motoko_rts::types::{Value, Words};

pub struct TestMemory {
    heap: Box<[u8]>,
    hp: usize,
}

impl TestMemory {
    pub fn new(size: Words<u32>) -> TestMemory {
        let bytes = size.to_bytes().0;
        let heap = vec![0u8; bytes as usize].into_boxed_slice();
        let hp = heap.as_ptr() as usize;
        TestMemory { heap, hp }
    }

    unsafe fn grow_memory(&mut self, ptr: usize) {
        let heap_end = self.heap.as_ptr() as usize + self.heap.len();
        if ptr > heap_end {
            // We don't allow growing memory in tests, allocate large enough for the test
            panic!(
                "TestMemory::grow_memory called: heap_end={:#x}, grow_memory argument={:#x}",
                heap_end, ptr
            );
        }
    }
}

impl Memory for TestMemory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.hp;
        let new_hp = old_hp + bytes.0 as usize;
        self.hp = new_hp;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        Value::from_ptr(old_hp)
    }
}
