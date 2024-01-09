use crate::{
    memory::{alloc_blob, Memory},
    stabilization::{
        deserialization::stable_memory_access::StableMemoryAccess,
        layout::checked_to_u32,
        serialization::stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
    },
    types::{size_of, Blob, Bytes, Value, TAG_BLOB},
};

use super::{
    checked_to_usize, round_to_u64, write_padding_u64, Serializer, StableToSpace, StableValue,
    StaticScanner,
};

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

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut Blob,
    ) {
        let byte_length = main_object.len().as_usize();
        stable_memory.raw_write(main_object.payload_addr() as usize, byte_length);
        write_padding_u64(stable_memory, byte_length);
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(self.byte_length);
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let blob_length = checked_to_u32(self.byte_length);
        alloc_blob(main_memory, Bytes(blob_length))
    }

    unsafe fn deserialize_static_part(&self, target_blob: *mut Blob) {
        debug_assert_eq!((*target_blob).header.tag, TAG_BLOB);
        debug_assert_eq!(
            (*target_blob).len.as_u32(),
            checked_to_u32(self.byte_length)
        );
    }

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_blob: *mut Blob,
    ) {
        let stable_address = stable_object.payload_address();
        let source_payload = stable_address + size_of::<StableBlob>().to_bytes().as_usize() as u64;
        let target_payload = target_blob.payload_addr() as usize;
        let blob_length = checked_to_u32(self.byte_length);
        stable_memory.raw_read(source_payload, target_payload, blob_length as usize);
    }
}
