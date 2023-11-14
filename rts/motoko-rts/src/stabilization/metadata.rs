//! Graph-copy-bazed serialization format:
//!
//! -- Stable memory
//! Raw stable memory or region data, size S in pages
//! -- Stable variables
//! Serialized data address N:
//!   Serialized object graph, length L
//!   (possible zero padding)
//! Type descriptor address M:
//!   Candid type table
//!     Byte length (u32)
//!     Data
//!   Type offset table
//!     Byte length (u32)
//!     Data
//!   (possible zero padding)
//! -- Last physical page (metadata):
//!   Version 3 (u32)
//!   (zero padding u32)
//!   Stable memory size in pages S (u64)
//!   Serialized data address N (u64)
//!   Serialized data length L (u64)
//!   Type descriptor address M (u64)
//! -- page end

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    stable_mem::{
        grow, ic0_stable64_read, ic0_stable64_size, ic0_stable64_write, read_u32, size, write_u32,
        write_u8, PAGE_SIZE,
    },
    types::{size_of, Bytes, Value},
};

use super::compatibility::TypeDescriptor;

const STABLE_GRAPH_COPY_VERSION: u32 = 3;

#[repr(C)]
#[derive(Default)]
struct LastPageRecord {
    version: u32,
    _padding: u32,
    stable_memory_pages: u64,
    serialized_data_address: u64,
    serialized_data_length: u64,
    type_descriptor_address: u64,
}

pub struct StabilizationMetadata {
    pub stable_memory_pages: u64,
    pub serialized_data_start: u64,
    pub serialized_data_length: u64,
    pub type_descriptor: TypeDescriptor,
}

impl StabilizationMetadata {
    fn ensure_space(offset: u64, length: u64) {
        let required_pages = (offset + length + PAGE_SIZE - 1) / PAGE_SIZE;
        let available_pages = size();
        if required_pages > available_pages {
            let additional_pages = available_pages - required_pages;
            assert_ne!(additional_pages, u64::MAX);
            let result = grow(additional_pages);
            assert_eq!(result, additional_pages);
        }
    }

    fn write_length(offset: &mut u64, length: u32) {
        Self::ensure_space(*offset, length as u64);
        write_u32(*offset, length);
        *offset += size_of::<u32>().to_bytes().as_usize() as u64;
    }

    fn write_blob(offset: &mut u64, value: Value) {
        unsafe {
            let length = value.as_blob().len().as_u32();
            Self::write_length(offset, length);
            Self::ensure_space(*offset, length as u64);
            ic0_stable64_write(
                *offset,
                value.as_blob().payload_const() as u64,
                length as u64,
            );
            *offset += length as u64;
        }
    }

    fn align_page_start(offset: &mut u64) {
        if *offset % PAGE_SIZE != 0 {
            let remainder = PAGE_SIZE - *offset % PAGE_SIZE;
            Self::fill_zero(offset, remainder);
        }
    }

    fn fill_zero(offset: &mut u64, length: u64) {
        // TODO: Optimize bulk zero write
        for _ in 0..length {
            write_u8(*offset, 0u8);
        }
        *offset += length;
    }

    fn save_type_descriptor(offset: &mut u64, descriptor: &TypeDescriptor) {
        assert_eq!(descriptor.main_actor_index, 0);
        Self::write_blob(offset, descriptor.candid_data);
        Self::write_blob(offset, descriptor.type_offsets);
    }

    fn read_length(offset: &mut u64) -> u32 {
        let length = read_u32(*offset);
        *offset += size_of::<u32>().to_bytes().as_usize() as u64;
        length
    }

    fn read_blob<M: Memory>(mem: &mut M, offset: &mut u64) -> Value {
        let length = Self::read_length(offset);
        unsafe {
            let value = alloc_blob(mem, Bytes(length));
            ic0_stable64_read(
                value.as_blob_mut().payload_addr() as u64,
                *offset,
                length as u64,
            );
            allocation_barrier(value);
            *offset += length as u64;
            value
        }
    }

    fn load_type_descriptor<M: Memory>(mem: &mut M, offset: &mut u64) -> TypeDescriptor {
        let candid_data = Self::read_blob(mem, offset);
        let type_offsets = Self::read_blob(mem, offset);
        TypeDescriptor {
            candid_data,
            type_offsets,
            main_actor_index: 0,
        }
    }

    fn write_last_physical_page<T>(offset: &mut u64, value: &T) {
        Self::align_page_start(offset);
        let last_physical_page = unsafe { ic0_stable64_size() };
        *offset = last_physical_page * PAGE_SIZE;
        let size = size_of::<T>().to_bytes().as_usize() as u64;
        Self::ensure_space(*offset, size);
        unsafe {
            ic0_stable64_write(*offset, value as *const T as u64, size);
        }
        *offset += size;
    }

    fn read_last_physical_page<T: Default>() -> T {
        let last_physical_page = unsafe { ic0_stable64_size() };
        let offset = last_physical_page * PAGE_SIZE;
        let size = size_of::<T>().to_bytes().as_usize() as u64;
        let mut value = T::default();
        unsafe {
            ic0_stable64_read(&mut value as *mut T as u64, offset, size);
        }
        value
    }

    pub fn store(&self) {
        assert!(self.stable_memory_pages * PAGE_SIZE <= self.serialized_data_start);
        let mut offset = self.serialized_data_start + self.serialized_data_length;
        Self::align_page_start(&mut offset);
        let type_descriptor_address = offset;
        Self::save_type_descriptor(&mut offset, &self.type_descriptor);
        let metadata = LastPageRecord {
            version: STABLE_GRAPH_COPY_VERSION,
            _padding: 0,
            stable_memory_pages: self.stable_memory_pages,
            serialized_data_address: self.serialized_data_start,
            serialized_data_length: self.serialized_data_length,
            type_descriptor_address,
        };
        Self::write_last_physical_page(&mut offset, &metadata);
    }

    pub fn load<M: Memory>(mem: &mut M) -> StabilizationMetadata {
        let metadata = Self::read_last_physical_page::<LastPageRecord>();
        assert_eq!(metadata.version, STABLE_GRAPH_COPY_VERSION);
        assert!(metadata.stable_memory_pages * PAGE_SIZE <= metadata.serialized_data_address);
        let mut offset = metadata.type_descriptor_address;
        let type_descriptor = Self::load_type_descriptor(mem, &mut offset);
        StabilizationMetadata {
            stable_memory_pages: metadata.stable_memory_pages,
            serialized_data_start: metadata.serialized_data_address,
            serialized_data_length: metadata.serialized_data_length,
            type_descriptor,
        }
    }
}
