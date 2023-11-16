//! Graph-copy-bazed serialization format:
//!
//! (Very first word is zeroed and backed up in last page)
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
//!   (zero padding to align at page end)
//!   Serialized data address N (u64)
//!   Serialized data length L (u64)
//!   Type descriptor address M (u64)
//!   Stable memory size in pages S (u64)
//!   First word of page 0
//!   Version 3 or 4 (u32)
//! -- page end

use core::cmp::max;

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    region::{VERSION_GRAPH_COPY_NO_REGIONS, VERSION_GRAPH_COPY_REGIONS},
    stable_mem::{
        get_version, ic0_stable64_read, ic0_stable64_size, ic0_stable64_write, read_u32,
        set_version, write_u32, PAGE_SIZE,
    },
    types::{size_of, Bytes, Value},
};

use super::{clear_stable_memory, compatibility::TypeDescriptor, grant_stable_space};

#[repr(C)]
#[derive(Default)]
struct LastPageRecord {
    serialized_data_address: u64,
    serialized_data_length: u64,
    type_descriptor_address: u64,
    stable_memory_pages: u64,
    first_word_backup: u32,
    version: u32,
}

pub struct StabilizationMetadata {
    pub stable_memory_pages: u64,
    pub serialized_data_start: u64,
    pub serialized_data_length: u64,
    pub type_descriptor: TypeDescriptor,
}

impl StabilizationMetadata {
    fn ensure_space(offset: u64, length: u64) {
        grant_stable_space(offset + length);
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
            clear_stable_memory(*offset, remainder);
            *offset += remainder;
        }
    }

    fn save_type_descriptor(offset: &mut u64, descriptor: &TypeDescriptor) {
        assert_eq!(descriptor.main_actor_index, 0);
        Self::write_blob(offset, descriptor.candid_data);
        Self::write_blob(offset, descriptor.type_offsets);
    }

    fn read_length(offset: &mut u64) -> u32 {
        let length = read_u32(*offset);
        clear_stable_memory(*offset, size_of::<u32>().to_bytes().as_usize() as u64);
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
            clear_stable_memory(*offset, length as u64);
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

    fn metadata_location() -> u64 {
        let physical_pages = unsafe { ic0_stable64_size() };
        assert!(physical_pages > 0);
        let last_page_start = (physical_pages - 1) * PAGE_SIZE;
        let size = size_of::<LastPageRecord>().to_bytes().as_usize() as u64;
        assert!(size < PAGE_SIZE);
        last_page_start + (PAGE_SIZE - size)
    }

    fn write_metadata(value: &LastPageRecord) {
        let offset = Self::metadata_location();
        let size = size_of::<LastPageRecord>().to_bytes().as_usize() as u64;
        Self::ensure_space(offset, size);
        unsafe {
            ic0_stable64_write(offset, value as *const LastPageRecord as u64, size);
        }
    }

    fn read_metadata() -> LastPageRecord {
        let offset = Self::metadata_location();
        let size = size_of::<LastPageRecord>().to_bytes().as_usize() as u64;
        let mut value = LastPageRecord::default();
        unsafe {
            ic0_stable64_read(&mut value as *mut LastPageRecord as u64, offset, size);
        }
        value
    }

    fn clear_metadata() {
        Self::write_metadata(&LastPageRecord::default());
    }

    pub fn store(&self) {
        assert!(self.stable_memory_pages * PAGE_SIZE <= self.serialized_data_start);
        let mut offset = self.serialized_data_start + self.serialized_data_length;
        Self::align_page_start(&mut offset);
        let type_descriptor_address = offset;
        Self::save_type_descriptor(&mut offset, &self.type_descriptor);
        Self::align_page_start(&mut offset);
        let first_word_backup = read_u32(0);
        // Clear very first word that is backed up in the last page.
        // This ensures compatibility with old legacy version 0 using no
        // experimental stable memory and no regions.
        write_u32(0, 0);
        let metadata = LastPageRecord {
            serialized_data_address: self.serialized_data_start,
            serialized_data_length: self.serialized_data_length,
            type_descriptor_address,
            stable_memory_pages: self.stable_memory_pages,
            first_word_backup,
            version: get_version(),
        };
        Self::write_metadata(&metadata);
    }

    pub fn load<M: Memory>(mem: &mut M) -> StabilizationMetadata {
        let metadata = Self::read_metadata();
        Self::clear_metadata();
        assert!(
            metadata.version == VERSION_GRAPH_COPY_NO_REGIONS
                || metadata.version == VERSION_GRAPH_COPY_REGIONS
        );
        set_version(metadata.version);
        write_u32(0, metadata.first_word_backup);
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

    pub fn matching_version() -> bool {
        let physical_pages = unsafe { ic0_stable64_size() };
        if physical_pages == 0 {
            // No stable memory -> Legacy version 0.
            return false;
        }
        if read_u32(0) != 0 {
            // Old stabilization with no experimental stable memory and no regions.
            // It stores non-zero marker at address 0 -> Legacy version 0.
            return false;
        }
        let address = physical_pages * PAGE_SIZE - size_of::<u32>().to_bytes().as_usize() as u64;
        let version = read_u32(address);
        assert!(version <= max(VERSION_GRAPH_COPY_NO_REGIONS, VERSION_GRAPH_COPY_REGIONS));
        version == VERSION_GRAPH_COPY_NO_REGIONS || version == VERSION_GRAPH_COPY_REGIONS
    }
}
