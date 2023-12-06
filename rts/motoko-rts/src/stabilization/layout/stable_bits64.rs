use crate::types::{lower32, upper32, Bits64, Value, TAG_BITS64};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableBits64 {
    bits: u64,
}

impl StaticScanner<StableValue> for StableBits64 {}

impl Serializer<Bits64> for StableBits64 {
    unsafe fn serialize_static_part(main_object: *mut Bits64) -> Self {
        StableBits64 {
            bits: (*main_object).bits(),
        }
    }

    unsafe fn deserialize_static_part(&self, target_bits64: *mut Bits64) {
        (*target_bits64).header.tag = TAG_BITS64;
        (*target_bits64)
            .header
            .init_forward(Value::from_ptr(target_bits64 as usize));
        (*target_bits64).bits_lo = lower32(self.bits);
        (*target_bits64).bits_hi = upper32(self.bits);
    }
}
