//! Read/write access on stable memory.
//! Experiment: No buffering.
//! Supporting Cheney's to-space in stable memory.

use core::mem::{size_of, zeroed};

use crate::stable_mem::{ic0_stable64_read, ic0_stable64_write};

use super::grant_stable_space;

/// Streamed reader/writer on stable memory.
/// Experiment: No buffering.
///
/// The memory supports two location-independent streams:
/// * Streamed reading and updating, used for scanning and patching pointers.
/// * Streamed writing, used for allocating new objects.
pub struct StableMemorySpace {
    /// The pointers used in the serialized stable memory layout are
    /// relative to this start address of the to-space.
    base_address: u64,
    /// Used for reading and updating.
    scan_address: u64,
    /// Used for writing.
    free_address: u64,
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

impl StableMemorySpace {
    pub fn open(start_address: u64) -> StableMemorySpace {
        StableMemorySpace {
            base_address: start_address,
            scan_address: start_address,
            free_address: start_address,
            closed: false,
        }
    }

    pub fn close(&mut self) {
        self.closed = true;
        debug_assert!(self.scan_address <= self.free_address);
    }

    /// Size of this memory space. Also serves to determine the addresses of a subsequently
    /// written object in the serialized stable memory format.
    pub fn written_length(&self) -> u64 {
        debug_assert!(self.base_address <= self.free_address);
        self.free_address - self.base_address
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
        debug_assert!(self.scan_address + length as u64 <= self.free_address);
        unsafe {
            ic0_stable64_read(data_address as u64, self.scan_address, length as u64);
        }
    }

    fn rewind(&mut self, length: usize) {
        debug_assert!(!self.closed);
        debug_assert!(length as u64 <= self.scan_address);
        self.scan_address -= length as u64;
    }

    fn skip(&mut self, length: usize) {
        debug_assert!(!self.closed);
        debug_assert!(self.scan_address + length as u64 <= self.free_address);
        self.scan_address += length as u64;
    }

    fn update<T>(&mut self, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_update(value_address, length);
    }

    fn raw_update(&mut self, data_address: usize, length: usize) {
        debug_assert!(length as u64 <= self.scan_address);
        self.scan_address -= length as u64;
        unsafe {
            ic0_stable64_write(self.scan_address, data_address as u64, length as u64);
        }
    }
}

impl WriteStream for StableMemorySpace {
    fn write<T>(&mut self, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_write(value_address, length);
    }

    fn raw_write(&mut self, data_address: usize, length: usize) {
        unsafe {
            grant_stable_space(self.free_address + length as u64);
            ic0_stable64_write(self.free_address, data_address as u64, length as u64);
        }
    }
}

impl Drop for StableMemorySpace {
    fn drop(&mut self) {
        debug_assert!(self.closed);
    }
}
