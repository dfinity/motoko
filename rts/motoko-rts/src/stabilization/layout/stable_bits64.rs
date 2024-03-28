use crate::{
    stabilization::serialization::stable_memory_stream::StableMemoryStream,
    types::{Bits64, Value, TAG_BITS64},
};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableBits64 {
    bits: u64,
}

impl StaticScanner<StableValue> for StableBits64 {}

impl Serializer<Bits64> for StableBits64 {
    unsafe fn serialize_static_part(
        _stable_memory: &mut StableMemoryStream,
        main_object: *mut Bits64,
    ) -> Self {
        StableBits64 {
            bits: (*main_object).bits as u64,
        }
    }

    unsafe fn deserialize_static_part(&self, target_bits64: *mut Bits64) {
        (*target_bits64).header.tag = TAG_BITS64;
        (*target_bits64)
            .header
            .init_forward(Value::from_ptr(target_bits64 as usize));
        (*target_bits64).bits = self.bits as usize;
    }
}
