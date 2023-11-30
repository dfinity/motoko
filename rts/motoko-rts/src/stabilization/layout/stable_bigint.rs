use crate::memory::Memory;
use crate::stabilization::layout::{checked_to_usize, write_padding_u64};
use crate::stabilization::stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream};
use crate::stabilization::StableMemoryAccess;
use crate::tommath_bindings::mp_int;
use crate::types::{size_of, BigInt, Bytes, Value, TAG_BIGINT};

use super::{checked_to_u32, round_to_u64, Serializer, StableToSpace, StableValue, StaticScanner};

// TODO: Use a format that is independent of Tom's math library, e.g. by using LEB128/SLEB128 encoding.

#[repr(C)]
pub struct StableBigInt {
    mp_int: mp_int,
    // Dynamically sized payload of `BigInt::data_length` bytes.
    // Zero padding to align to next `u64`.
}

impl StableBigInt {
    fn length(&self) -> u64 {
        let mp_int = &self.mp_int as *const mp_int;
        unsafe { BigInt::data_length(mp_int).as_u32() as u64 }
    }
}

impl StaticScanner<StableValue> for StableBigInt {}

impl Serializer<BigInt> for StableBigInt {
    unsafe fn serialize_static_part(main_object: *mut BigInt) -> Self {
        StableBigInt {
            mp_int: (*main_object).mp_int,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemoryStream, main_object: *mut BigInt) {
        let byte_length = main_object.len().as_usize();
        memory.raw_write(main_object.payload_addr() as usize, byte_length);
        write_padding_u64(memory, byte_length);
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        _translate: &F,
    ) {
        let rounded_length = round_to_u64(self.length());
        context.to_space().skip(checked_to_usize(rounded_length));
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let payload_length = Bytes(checked_to_u32(self.length()));
        let total_size = size_of::<BigInt>() + payload_length.to_words();
        main_memory.alloc_words(total_size)
    }

    unsafe fn deserialize_static_part(&self, target_bigint: *mut BigInt) {
        (*target_bigint).header.tag = TAG_BIGINT;
        (*target_bigint)
            .header
            .init_forward(Value::from_ptr(target_bigint as usize));
        (*target_bigint).mp_int = self.mp_int;
    }

    unsafe fn deserialize_dynamic_part(
        &self,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_bigint: *mut BigInt,
    ) {
        let stable_address = stable_object.payload_address();
        let source_payload =
            stable_address + size_of::<StableBigInt>().to_bytes().as_usize() as u64;
        let target_payload = target_bigint.payload_addr() as usize;
        let payload_length = Bytes(checked_to_u32(self.length()));
        stable_memory.raw_read(source_payload, target_payload, payload_length.as_usize());
    }
}
