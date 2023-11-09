//! Buffered streamed reader/writer to stable memory. Optimized for the
//! to-space in Cheney's algorithm, employed in the graph-copying-based
//! stabilization.
//!
//! The buffer supports streamed reading and writing, each at an independent
//! position, while buffering the pages that are read and/or written.
//!
//! Streamed reading is used for the `scan`-pointer in Cheney's algorithm.
//! Stream writing is used for the `free`-pointer in Cheney's algorithm.

use core::cmp::min;

use crate::{
    mem_utils::memcpy_bytes,
    rts_trap_with,
    stable_mem::{self, ic0_stable64_read, ic0_stable64_write},
    types::{size_of, Bytes},
};

pub const PAGE_SIZE: u64 = 64 * 1024;
const FREE_PAGE_INDEX: u64 = u64::MAX;

struct StableMemoryPage {
    page_index: u64,
    modified: bool,
    data: [u8; PAGE_SIZE as usize],
}

impl StableMemoryPage {
    fn new() -> StableMemoryPage {
        StableMemoryPage {
            page_index: FREE_PAGE_INDEX,
            modified: false,
            data: [0u8; PAGE_SIZE as usize],
        }
    }

    fn is_free(&self) -> bool {
        self.page_index == FREE_PAGE_INDEX
    }

    fn cached_data(&mut self) -> *mut u8 {
        &mut self.data as *mut u8
    }

    fn stable_memory_offset(&self) -> u64 {
        self.page_index * PAGE_SIZE
    }

    fn swap_out(&mut self) {
        assert!(!self.is_free());
        if self.modified {
            unsafe {
                ic0_stable64_write(
                    self.stable_memory_offset(),
                    self.cached_data() as u64,
                    PAGE_SIZE,
                );
            }
        }
        self.page_index = FREE_PAGE_INDEX;
        self.modified = false;
    }

    fn swap_in(&mut self, page_index: u64) {
        assert!(self.is_free());
        Self::grow_if_needed(page_index);
        self.page_index = page_index;
        assert!(!self.modified);
        unsafe {
            ic0_stable64_read(
                self.cached_data() as u64,
                self.stable_memory_offset(),
                PAGE_SIZE,
            )
        }
    }

    fn grow_if_needed(page_index: u64) {
        let total_pages = stable_mem::size();
        if page_index > total_pages {
            let result = stable_mem::grow(total_pages - page_index);
            if result == u64::MAX {
                unsafe {
                    rts_trap_with("Insufficient stable memory");
                }
            }
        }
    }
}

// At most two pages are needed to cache the current streamed read and write location.
const CACHE_SIZE: usize = 2;

pub struct StableMemoryReaderWriter {
    read_address: u64,
    write_address: u64,
    // TODO: Use a way to not allocate a 128K empty data segment.
    cache: [StableMemoryPage; CACHE_SIZE],
    closed: bool,
}

enum AccessMode {
    Read,
    Write,
}

impl StableMemoryReaderWriter {
    pub fn open(start_address: u64) -> StableMemoryReaderWriter {
        StableMemoryReaderWriter {
            read_address: start_address,
            write_address: start_address,
            cache: [StableMemoryPage::new(), StableMemoryPage::new()],
            closed: false,
        }
    }

    pub fn close(&mut self) -> u64 {
        for page in &mut self.cache {
            if !page.is_free() {
                page.swap_out();
            }
        }
        self.closed = true;
        debug_assert!(self.read_address <= self.write_address);
        self.write_address
    }

    pub fn reading_finished(&self) -> bool {
        debug_assert!(self.read_address <= self.write_address);
        self.read_address == self.write_address
    }

    fn can_evict(&self, page_index: u64) -> bool {
        page_index != self.read_address / PAGE_SIZE && page_index != self.write_address / PAGE_SIZE
    }

    fn lookup(&mut self, page_index: u64) -> &mut StableMemoryPage {
        for cache_index in 0..self.cache.len() {
            if self.cache[cache_index].page_index == page_index {
                return &mut self.cache[cache_index];
            }
            if self.can_evict(self.cache[cache_index].page_index) {
                let page = &mut self.cache[cache_index];
                if !page.is_free() {
                    page.swap_out();
                }
            }
        }


        for page in &mut self.cache {
            if page.is_free() {
                page.swap_in(page_index);
                return page;
            }
        }
        unreachable!()
    }

    fn chunked_access(&mut self, mode: AccessMode, main_memory_address: &mut usize, length: usize) {
        assert!(!self.closed);
        let mut stable_memory_address = match mode {
            AccessMode::Read => self.read_address,
            AccessMode::Write => self.write_address,
        };
        let access_end = stable_memory_address + length as u64;
        while stable_memory_address < access_end {
            let page_index = stable_memory_address / PAGE_SIZE;
            let page_offset = stable_memory_address % PAGE_SIZE;
            let page_end = (page_index + 1) * PAGE_SIZE;
            let chunk_end = min(access_end, page_end);
            let chunk_size = chunk_end - stable_memory_address;
            let page = self.lookup(page_index);
            let cache_address = page.cached_data() as usize + page_offset as usize;
            let (destination, source) = match mode {
                AccessMode::Read => (*main_memory_address, cache_address),
                AccessMode::Write => (cache_address, *main_memory_address),
            };
            unsafe {
                memcpy_bytes(destination, source, Bytes(chunk_size as u32));
            }
            match mode {
                AccessMode::Read => {},
                AccessMode::Write => page.modified = true,
            }
            stable_memory_address += chunk_size;
            match mode {
                AccessMode::Read => self.read_address = stable_memory_address,
                AccessMode::Write => self.write_address = stable_memory_address,
            }
            *main_memory_address += chunk_size as usize;
        }
    }

    pub fn read<T>(&mut self, value: &mut T) {
        let length = size_of::<T>().to_bytes().as_usize();
        assert!(self.read_address + length as u64 <= self.write_address);
        let mut value_address = value as *mut T as usize;
        self.chunked_access(AccessMode::Read, &mut value_address, length);
    }

    pub fn write<T>(&mut self, value: &T) {
        let length = size_of::<T>().to_bytes().as_usize();
        let mut value_address = value as *const T as usize;
        self.chunked_access(AccessMode::Write, &mut value_address, length);
    }
}

impl Drop for StableMemoryReaderWriter {
    fn drop(&mut self) {
        assert!(self.closed);
    }
}
