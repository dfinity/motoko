use crate::types::{Bits32, Value, TAG_BITS32};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableBits32 {
    bits: u32,
}

impl StaticScanner<StableValue> for StableBits32 {}

impl Serializer<Bits32> for StableBits32 {
    unsafe fn serialize_static_part(main_object: *mut Bits32) -> Self {
        StableBits32 {
            bits: (*main_object).bits,
        }
    }

    unsafe fn deserialize_static_part(&self, target_bits32: *mut Bits32) {
        (*target_bits32).header.tag = TAG_BITS32;
        (*target_bits32)
            .header
            .init_forward(Value::from_ptr(target_bits32 as usize));
        (*target_bits32).bits = self.bits;
    }
}
