//! Random read/write access to stable memory.
//! Supporting Cheney's from-space in stable memory.

use core::mem::{size_of, MaybeUninit};

use crate::stable_mem::{ic0_stable64_read, ic0_stable64_write};

use super::BUFFER_SIZE;

/// Random access to stable memory.
/// Used for the from-space during destabilization.
pub struct StableMemoryAccess {
    base_address: u64,
    length: u64,
}

impl StableMemoryAccess {
    pub fn open(base_address: u64, length: u64) -> StableMemoryAccess {
        StableMemoryAccess {
            base_address,
            length,
        }
    }

    pub fn read<T>(&self, source_offset: u64) -> T {
        let length = size_of::<T>();
        let mut value = unsafe { MaybeUninit::<T>::uninit().assume_init() };
        let value_address = &mut value as *mut T as usize;
        self.raw_read(source_offset, value_address, length);
        value
    }

    pub fn raw_read(&self, source_offset: u64, target_address: usize, length: usize) {
        debug_assert!(source_offset + length as u64 <= self.length);
        unsafe {
            ic0_stable64_read(
                target_address as u64,
                self.base_address + source_offset,
                length as u64,
            );
        }
    }

    pub fn write<T>(&mut self, target_offset: u64, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_write(target_offset, value_address, length);
    }

    pub fn raw_write(&mut self, target_offset: u64, source_address: usize, length: usize) {
        debug_assert!(target_offset + length as u64 <= self.length);
        unsafe {
            ic0_stable64_write(
                self.base_address + target_offset,
                source_address as u64,
                length as u64,
            );
        }
    }
}

/// Optimization: Buffered writer of a sequence of elements.
/// Used for writing array elements and object fields.
pub fn read_series<T: Copy, F: Fn(u64, T)>(
    stable_memory: &StableMemoryAccess,
    source_offset: u64,
    count: u64,
    set_item: &F,
) {
    let mut buffer = unsafe { [MaybeUninit::<T>::uninit().assume_init(); BUFFER_SIZE] };
    let mut offset = 0;
    while offset < count {
        let chunk_size = core::cmp::min(BUFFER_SIZE as u64, count - offset) as usize;
        let read_address = source_offset + offset * size_of::<T>() as u64;
        stable_memory.raw_read(
            read_address,
            &mut buffer as *mut T as usize,
            chunk_size * size_of::<T>(),
        );
        for index in 0..chunk_size {
            set_item(offset + index as u64, buffer[index]);
        }
        offset += chunk_size as u64;
    }
}
