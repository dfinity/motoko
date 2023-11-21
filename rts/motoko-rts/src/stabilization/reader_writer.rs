//! Buffered read/write access on stable memory.
//! Supporting Cheney's to-space in stable memory.

use core::{
    cmp::min,
    mem::{size_of, zeroed},
};

use crate::{
    mem_utils::memcpy_bytes,
    stable_mem::{ic0_stable64_read, ic0_stable64_write, PAGE_SIZE},
    types::Bytes,
};

use super::grant_stable_space;

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
        grant_stable_space((page_index + 1) * PAGE_SIZE);
    }
}

// At most two pages are needed to cache the current streamed `scan`` and `free` position.
const CACHE_SIZE: usize = 2;

/// Buffered streamed reader/writer on stable memory.
/// Optimized for Cheney's algorithm.
///
/// The buffer supports two location-independent streams:
/// * Streamed reading and updating, used for scanning and patching pointers.
/// * Streamed writing, used for allocating new objects.
pub struct StableMemorySpace {
    /// The pointers used in the serialized stable memory layout are
    /// relative to this start address of the to-space.
    base_address: u64,
    /// Used fore reading and updating.
    scan_address: u64,
    /// Used for writing.
    free_address: u64,
    // TODO: Use a way to not allocate a 128K empty data segment.
    cache: [StableMemoryPage; CACHE_SIZE],
    closed: bool,
}

pub trait ScanStream {
    // Determines whether the stream has reached the end.
    fn scan_completed(&self) -> bool;
    // Read a value from the stream.
    fn read<T>(&mut self) -> T;
    // Read raw data from the stream.
    fn raw_read(&mut self, data_address: usize, length: usize);
    // Go back in the stream.
    fn rewind(&mut self, length: usize);
    // Skip data in the stream.
    fn skip(&mut self, length: usize);
    // Overwrite the value right before the stream position.
    fn update<T>(&mut self, value: &T);
    // Overwrite raw data right before the stream position.
    fn raw_update(&mut self, data_address: usize, length: usize);
}

pub trait WriteStream {
    // Append a value at the stream end.
    fn write<T>(&mut self, value: &T);
    // Append raw data at the stream end.
    fn raw_write(&mut self, data_address: usize, length: usize);
}

enum AccessMode {
    /// Read at `scan`-position.
    Read,
    /// Update at `scan`-position.
    Update,
    /// Write at `free`-position.
    Write,
}

impl StableMemorySpace {
    pub fn open(start_address: u64) -> StableMemorySpace {
        StableMemorySpace {
            base_address: start_address,
            scan_address: start_address,
            free_address: start_address,
            cache: [StableMemoryPage::new(), StableMemoryPage::new()],
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
        debug_assert!(self.scan_address <= self.free_address);
    }

    /// Size of this memory space. Also serves to determine the addresses of a subsequently
    /// written object in the serialized stable memory format.
    pub fn written_length(&self) -> u64 {
        debug_assert!(self.base_address <= self.free_address);
        self.free_address - self.base_address
    }

    fn can_evict(&self, page_index: u64) -> bool {
        page_index != self.scan_address / PAGE_SIZE && page_index != self.free_address / PAGE_SIZE
    }

    fn lookup(&mut self, page_index: u64) -> &mut StableMemoryPage {
        assert!(!self.can_evict(page_index));
        for position in 0..self.cache.len() {
            let cached_page_index = self.cache[position].page_index;
            if cached_page_index == page_index {
                return &mut self.cache[position];
            }
            if self.can_evict(cached_page_index) {
                let page = &mut self.cache[position];
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

    fn chunked_access(&mut self, mode: AccessMode, main_memory_address: usize, length: usize) {
        assert!(!self.closed);
        let mut main_memory_address = main_memory_address;
        let mut stable_memory_address = match mode {
            AccessMode::Read | AccessMode::Update => self.scan_address,
            AccessMode::Write => self.free_address,
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
                AccessMode::Read => (main_memory_address, cache_address),
                AccessMode::Update | AccessMode::Write => (cache_address, main_memory_address),
            };
            unsafe {
                memcpy_bytes(destination, source, Bytes(chunk_size as u32));
            }
            match mode {
                AccessMode::Read => {}
                AccessMode::Update | AccessMode::Write => page.modified = true,
            }
            stable_memory_address += chunk_size;
            match mode {
                AccessMode::Read | AccessMode::Update => self.scan_address = stable_memory_address,
                AccessMode::Write => self.free_address = stable_memory_address,
            }
            main_memory_address += chunk_size as usize;
        }
    }
}

impl ScanStream for StableMemorySpace {
    fn scan_completed(&self) -> bool {
        debug_assert!(self.scan_address <= self.free_address);
        self.scan_address == self.free_address
    }

    fn read<T>(&mut self) -> T {
        let length = size_of::<T>();
        let mut value = unsafe { zeroed::<T>() };
        let value_address = &mut value as *mut T as usize;
        self.raw_read(value_address, length);
        value
    }

    fn raw_read(&mut self, data_address: usize, length: usize) {
        assert!(self.scan_address + length as u64 <= self.free_address);
        self.chunked_access(AccessMode::Read, data_address, length);
    }

    fn rewind(&mut self, length: usize) {
        assert!(!self.closed);
        assert!(length as u64 <= self.scan_address);
        self.scan_address -= length as u64;
    }

    fn skip(&mut self, length: usize) {
        assert!(!self.closed);
        assert!(self.scan_address + length as u64 <= self.free_address);
        self.scan_address += length as u64;
    }

    fn update<T>(&mut self, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_update(value_address, length);
    }

    fn raw_update(&mut self, data_address: usize, length: usize) {
        assert!(length as u64 <= self.scan_address);
        self.scan_address -= length as u64;
        self.chunked_access(AccessMode::Update, data_address, length);
    }
}

impl WriteStream for StableMemorySpace {
    fn write<T>(&mut self, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_write(value_address, length);
    }

    fn raw_write(&mut self, data_address: usize, length: usize) {
        self.chunked_access(AccessMode::Write, data_address, length);
    }
}

impl Drop for StableMemorySpace {
    fn drop(&mut self) {
        assert!(self.closed);
    }
}
