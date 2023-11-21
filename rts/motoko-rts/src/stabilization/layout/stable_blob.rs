use crate::{
    stabilization::reader_writer::{ScanStream, StableMemorySpace, WriteStream},
    types::{Blob, Bytes, Value},
};

use super::{
    checked_to_u32, checked_to_usize, round_to_u64, write_padding_u64, Serializer, StableValue,
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

impl StableBlob {
    unsafe fn payload_address(self: *mut Self) -> *mut u8 {
        self.add(1) as *mut u8
    }

    unsafe fn payload_length(self: *const Self) -> u64 {
        self.read_unaligned().byte_length
    }
}

impl StaticScanner<StableValue> for StableBlob {}
impl StaticScanner<Value> for Blob {}

impl Serializer<Blob> for StableBlob {
    unsafe fn serialize_static_part(main_object: *mut Blob) -> Self {
        StableBlob {
            byte_length: main_object.len().as_usize() as u64,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemorySpace, main_object: *mut Blob) {
        let byte_length = main_object.len().as_usize();
        memory.raw_write(main_object.payload_addr() as usize, byte_length);
        write_padding_u64(memory, byte_length);
    }

    fn scan_serialized_dynamic<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, StableValue) -> StableValue,
    >(
        context: &mut C,
        stable_object: &Self,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(stable_object.byte_length);
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Blob {
        let length = Bytes(checked_to_u32(stable_object.read_unaligned().byte_length));
        Blob::new(target_address, length)
    }

    unsafe fn deserialize_dynamic_part(memory: &mut StableMemorySpace, stable_object: *mut Self) {
        let byte_length = checked_to_u32(stable_object.payload_length());
        let rounded_length = Bytes(byte_length).to_words().to_bytes().as_usize();
        memory.raw_write(stable_object.payload_address() as usize, rounded_length)
    }

    fn scan_deserialized_dynamic<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, Value) -> Value,
    >(
        context: &mut C,
        main_object: &Blob,
        _translate: &F,
    ) {
        let rounded_length = main_object.len.to_words().to_bytes().as_usize();
        context.to_space().skip(rounded_length);
    }
}
