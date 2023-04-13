use std::{rc::Rc, cell::RefCell};

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

pub trait Memory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value;
}

impl Memory for TestMemory {
    unsafe fn alloc_words(&mut self, n: Words<u32>) -> Value {
        let bytes = n.to_bytes();

        // Update heap pointer
        let old_hp = self.hp;
        let new_hp = old_hp + bytes.as_usize();
        self.hp = new_hp;

        // Grow memory if needed
        self.grow_memory(new_hp as usize);

        Value::from_ptr(old_hp)
    }
}

static mut TEST_MEMORY: Option<Rc<RefCell<dyn Memory>>> = None;

pub unsafe fn set_memory<M: Memory + 'static>(memory: M) {
    TEST_MEMORY = Some(Rc::new(RefCell::new(memory)));
}

pub unsafe fn share_memory<M: Memory + 'static>(memory: Rc<RefCell<M>>) {
    TEST_MEMORY = Some(memory);
}

// Export back to the RTS for unit testing.
#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> Value {
    TEST_MEMORY.as_mut().unwrap().as_ref().borrow_mut().alloc_words(n)
}
