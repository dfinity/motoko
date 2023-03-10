use motoko_rts::memory::{Memory, Roots};
use motoko_rts::types::Words;

pub struct TestMemory {
    heap: Box<[u8]>,
    heap_base: usize,
    last_hp: usize,
    hp: usize,
}

impl TestMemory {
    pub fn new(size: Words<u32>) -> TestMemory {
        let bytes = size.to_bytes().as_usize();
        let heap = vec![0u8; bytes].into_boxed_slice();
        let heap_base = heap.as_ptr() as usize;
        let hp = heap.as_ptr() as usize;
        TestMemory {
            heap,
            heap_base,
            last_hp: hp,
            hp,
        }
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

    pub fn set_last_heap_pointer(&mut self, address: usize) {
        self.last_hp = address;
    }
}

impl Memory for TestMemory {
    fn get_roots(&self) -> Roots {
        unimplemented!()
    }

    fn get_heap_base(&self) -> usize {
        self.heap_base
    }

    fn get_last_heap_pointer(&self) -> usize {
        self.last_hp
    }

    fn get_heap_pointer(&self) -> usize {
        self.hp
    }

    unsafe fn shrink_heap(&mut self, _new_heap_pointer: usize) {
        unimplemented!()
    }

    unsafe fn set_heap_base(&mut self, new_heap_base: usize) {
        self.heap_base = new_heap_base;
    }

    unsafe fn alloc_words(&mut self, n: Words<u32>) -> usize {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.hp;
        let new_hp = old_hp + bytes.as_usize();
        self.hp = new_hp;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        old_hp
    }
}
