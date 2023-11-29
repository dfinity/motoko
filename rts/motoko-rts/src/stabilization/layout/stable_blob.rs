use crate::{
    stabilization::stable_memory_access::StableMemoryAccess,
    stabilization::{stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream}, layout::checked_to_u32},
    types::{Blob, Value, Bytes, size_of}, memory::alloc_blob, barriers::allocation_barrier,
};

use super::{
    checked_to_usize, round_to_u64, write_padding_u64, Serializer, StableToSpace, StableValue,
    StaticScanner,
};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
#[derive(Default)]
pub struct StableBlob {
    byte_length: u64,
    // Dynamically sized body with `byte_length` bytes. No pointers to be scanned.
    // Zero padding to align to next `u64`.
    // Note: The rounding of object sizes to at least 2 bytes is necessary for the skewed pointer representation.
}

impl StaticScanner<StableValue> for StableBlob {}

impl Serializer<Blob> for StableBlob {
    unsafe fn serialize_static_part(main_object: *mut Blob) -> Self {
        StableBlob {
            byte_length: main_object.len().as_usize() as u64,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemoryStream, main_object: *mut Blob) {
        let byte_length = main_object.len().as_usize();
        memory.raw_write(main_object.payload_addr() as usize, byte_length);
        write_padding_u64(memory, byte_length);
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        context: &mut C,
        stable_object: &Self,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(stable_object.byte_length);
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn deserialize<M: crate::memory::Memory>(
        main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let stable_address = stable_object.payload_address();
        let stable_blob = stable_memory.read::<StableBlob>(stable_address);
        let blob_length = checked_to_u32(stable_blob.byte_length);
        let target_object = alloc_blob(main_memory, Bytes(blob_length));
        let target_blob = target_object.as_blob_mut();
        let source_payload = stable_address + size_of::<StableBlob>().to_bytes().as_usize() as u64;
        let target_payload = target_blob.payload_addr() as usize;
        stable_memory.raw_read(source_payload, target_payload, blob_length as usize);
        allocation_barrier(target_object)
    }
}
