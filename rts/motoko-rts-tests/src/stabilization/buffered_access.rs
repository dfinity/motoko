use std::cell::RefCell;

use libc::{c_void, memcpy};
use motoko_rts::stabilization::buffered_access::{StableMemoryReaderWriter, PAGE_SIZE};

use crate::memory;

thread_local! {
    static STABLE_MEMORY: RefCell<Vec<u8>> = RefCell::new(vec![]);
}

#[no_mangle]
pub fn ic0_stable64_write(offset: u64, source: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        assert!(offset + size <= memory.borrow().len() as u64);
        let destination = memory.borrow_mut().as_mut_ptr() as *mut c_void;
        unsafe {
            memcpy(destination, source as *mut c_void, size as usize);
        }
    });
}

#[no_mangle]
pub fn ic0_stable64_read(destination: u64, offset: u64, size: u64) {
    STABLE_MEMORY.with(|memory| {
        println!("TEST {offset} {size} {}", memory.borrow().len());
        assert!(offset + size <= memory.borrow().len() as u64);
        let source = memory.borrow_mut().as_mut_ptr() as *mut c_void;
        unsafe { memcpy(destination as *mut c_void, source, size as usize) }
    });
}

#[no_mangle]
pub fn moc_stable_mem_size() -> u64 {
    STABLE_MEMORY.with(|memory| memory.borrow().len()) as u64
}

#[no_mangle]
pub fn moc_stable_mem_grow(additional_pages: u64) -> u64 {
    for _ in 0..additional_pages * PAGE_SIZE {
        STABLE_MEMORY.with(|memory| memory.borrow_mut().push(0));
    }
    additional_pages
}

pub unsafe fn test() {
    println!("  Testing buffered access ...");
    test_empy_reader_writer();
    test_simple_read_write();
}

fn test_empy_reader_writer() {
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    let size = reader_writer.close();
    assert_eq!(size, 0);
}

fn test_simple_read_write() {
    let mut reader_writer = StableMemoryReaderWriter::open(0);
    const NUMBER: u64 = 1234567890;
    reader_writer.write(&NUMBER);
    let mut result = 0u64;
    reader_writer.read(&mut result);
    assert_eq!(result, NUMBER);
}
