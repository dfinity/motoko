//! Buffered random access to stable memory.
//! Using page-based swapping to and from limited main-memory space.
//! The cached pages use an underlying blob for storing cached data in main memory.
//!
//! NOTE: This data structure uses raw pointers, meaning that no GC increment can
//! run during the usage of this structure.

use core::{
    array::from_fn,
    cmp::min,
    mem::{size_of, zeroed},
};

use crate::{
    mem_utils::memcpy_bytes,
    memory::{alloc_blob, Memory},
    stable_mem::{ic0_stable64_read, ic0_stable64_write, PAGE_SIZE},
    types::Bytes,
};

use super::grant_stable_space;

const FREE_PAGE_INDEX: u64 = u64::MAX;

struct StableMemoryPage {
    page_index: u64,
    modified: bool,
    data: *mut u8,
    hits: usize,
}

impl StableMemoryPage {
    fn new(data: *mut u8) -> StableMemoryPage {
        StableMemoryPage {
            page_index: FREE_PAGE_INDEX,
            modified: false,
            data,
            hits: 0,
        }
    }

    fn is_free(&self) -> bool {
        self.page_index == FREE_PAGE_INDEX
    }

    fn cached_data(&self) -> *mut u8 {
        self.data
    }

    fn stable_memory_offset(&self) -> u64 {
        self.page_index * PAGE_SIZE
    }

    fn swap_out(&mut self) {
        debug_assert!(!self.is_free());
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
        self.hits = 0;
    }

    fn swap_in(&mut self, page_index: u64) {
        debug_assert!(self.is_free());
        Self::grow_if_needed(page_index);
        self.page_index = page_index;
        debug_assert!(!self.modified);
        debug_assert_eq!(self.hits, 0);
        unsafe {
            ic0_stable64_read(
                self.cached_data() as u64,
                self.stable_memory_offset(),
                PAGE_SIZE,
            )
        }
    }

    fn grow_if_needed(page_index: u64) {
        grant_stable_space((page_index + 1) * PAGE_SIZE);
    }
}

/// 4 MB cache capacity, 64 pages of 64 KB.
const NUMBER_OF_CACHED_PAGES: usize = 64;

/// Buffered random access to stable memory.
/// Only to be used when no GC increment runs in between.
pub struct BufferedStableMemory {
    /// The offsets used for accessing the buffer are relative
    /// this start address in stable memory.
    base_address: u64,
    cache: [StableMemoryPage; NUMBER_OF_CACHED_PAGES],
    closed: bool,
}

#[derive(PartialEq)]
enum AccessMode {
    Read,
    Write,
}

impl BufferedStableMemory {
    pub fn open<M: Memory>(mem: &mut M, base_address: u64) -> BufferedStableMemory {
        let cache_capacity = NUMBER_OF_CACHED_PAGES * PAGE_SIZE as usize;
        let cache_start = unsafe {
            let blob = alloc_blob(mem, Bytes(cache_capacity as u32));
            blob.as_blob_mut().payload_addr()
        };
        let cache = from_fn(|index| {
            let data = unsafe { cache_start.add(index * PAGE_SIZE as usize) };
            StableMemoryPage::new(data)
        });
        BufferedStableMemory {
            base_address,
            cache,
            closed: false,
        }
    }

    pub fn close(&mut self) {
        for page in &mut self.cache {
            if !page.is_free() {
                page.swap_out();
            }
        }
        self.closed = true;
    }

    fn lookup(&mut self, page_index: u64) -> &mut StableMemoryPage {
        let mut least_recently_used = 0;
        let mut least_hits = usize::MAX;
        // TODO: Optimize: Use a faster lookup than linear search.
        for position in 0..self.cache.len() {
            if self.cache[position].page_index == page_index {
                return &mut self.cache[position];
            }
            if self.cache[position].hits < least_hits {
                least_recently_used = position;
                least_hits = self.cache[position].hits;
            }
        }
        let page = &mut self.cache[least_recently_used];
        debug_assert_ne!(page.page_index, page_index);
        if !page.is_free() {
            page.swap_out();
        }
        page.swap_in(page_index);
        page
    }

    fn chunked_access(
        &mut self,
        mode: AccessMode,
        stable_memory_offset: u64,
        main_memory_address: usize,
        length: usize,
    ) {
        debug_assert!(!self.closed);
        debug_assert!(main_memory_address <= usize::MAX - length);
        debug_assert!(self.base_address < u64::MAX - stable_memory_offset);
        debug_assert!(self.base_address + stable_memory_offset <= u64::MAX - length as u64);
        let mut main_memory_address = main_memory_address;
        let mut stable_memory_address = self.base_address + stable_memory_offset;
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
                AccessMode::Read => (main_memory_address, cache_address),
                AccessMode::Write => (cache_address, main_memory_address),
            };
            unsafe {
                memcpy_bytes(destination, source, Bytes(chunk_size as u32));
            }
            if mode == AccessMode::Write {
                page.modified = true;
            }
            stable_memory_address += chunk_size;
            main_memory_address += chunk_size as usize;
        }
    }

    pub fn read<T>(&mut self, stable_memory_offset: u64) -> T {
        let length = size_of::<T>();
        let mut value = unsafe { zeroed::<T>() };
        let main_memory_address = &mut value as *mut T as usize;
        self.raw_read(stable_memory_offset, main_memory_address, length);
        value
    }

    pub fn raw_read(
        &mut self,
        stable_memory_offset: u64,
        main_memory_address: usize,
        length: usize,
    ) {
        self.chunked_access(
            AccessMode::Read,
            stable_memory_offset,
            main_memory_address,
            length,
        );
    }

    pub fn write<T>(&mut self, stable_memory_offset: u64, value: &T) {
        let length = size_of::<T>();
        let main_memory_address = value as *const T as usize;
        self.raw_write(stable_memory_offset, main_memory_address, length);
    }

    pub fn raw_write(
        &mut self,
        stable_memory_offset: u64,
        main_memory_address: usize,
        length: usize,
    ) {
        self.chunked_access(
            AccessMode::Write,
            stable_memory_offset,
            main_memory_address,
            length,
        );
    }
}

impl Drop for BufferedStableMemory {
    fn drop(&mut self) {
        debug_assert!(self.closed);
    }
}
