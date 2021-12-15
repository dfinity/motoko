use motoko_rts::memory::Memory;
use motoko_rts::types::{Value, Words};

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
    unsafe fn alloc_words(&mut self, n: Words<u32>, init_word: u32) -> Value {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.hp;
        let new_hp = old_hp + bytes.as_usize();
        self.hp = new_hp;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        *(old_hp as *mut u32) = init_word;
        debug_assert_eq!(Value::from_ptr(old_hp).as_obj().tag(), init_word);
        Value::from_ptr(old_hp)
    }
}
