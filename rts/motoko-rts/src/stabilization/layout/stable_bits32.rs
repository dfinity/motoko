use crate::types::{Bits32, Value};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
#[derive(Default)]
pub struct StableBits32 {
    bits: u32,
}

impl StaticScanner<StableValue> for StableBits32 {}
impl StaticScanner<Value> for Bits32 {}

impl Serializer<Bits32> for StableBits32 {
    unsafe fn serialize_static_part(main_object: *mut Bits32) -> Self {
        StableBits32 {
            bits: (*main_object).bits,
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Bits32 {
        let bits = stable_object.read_unaligned().bits;
        Bits32::new(target_address, bits)
    }
}
