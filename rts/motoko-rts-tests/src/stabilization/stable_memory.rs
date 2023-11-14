use std::cell::RefCell;

use libc::{c_void, memcpy};

const PAGE_SIZE: u64 = 64 * 1024;

thread_local! {
    static STABLE_MEMORY: RefCell<Vec<u8>> = RefCell::new(vec![]);
}

pub fn clear_stable_memory() {
    STABLE_MEMORY.with(|memory| {
        memory.borrow_mut().clear();
    })
}

#[no_mangle]
pub fn ic0_stable64_write(offset: u64, source: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        assert!(offset + size <= memory.borrow().len() as u64);
        let destination = memory.borrow_mut().as_mut_ptr() as u64 + offset;
        unsafe {
            memcpy(
                destination as *mut c_void,
                source as *mut c_void,
                size as usize,
            );
        }
    });
}

#[no_mangle]
pub fn ic0_stable64_read(destination: u64, offset: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        assert!(offset + size <= memory.borrow().len() as u64);
        let source = memory.borrow_mut().as_mut_ptr() as u64 + offset;
        unsafe {
            memcpy(
                destination as *mut c_void,
                source as *mut c_void,
                size as usize,
            );
        }
    });
}

#[no_mangle]
pub fn moc_stable_mem_get_size() -> u64 {
    STABLE_MEMORY.with(|memory| memory.borrow().len()) as u64 / PAGE_SIZE
}

#[no_mangle]
pub fn moc_stable_mem_grow(additional_pages: u64) -> u64 {
    for _ in 0..additional_pages * PAGE_SIZE {
        STABLE_MEMORY.with(|memory| memory.borrow_mut().push(0));
    }
    additional_pages
}
