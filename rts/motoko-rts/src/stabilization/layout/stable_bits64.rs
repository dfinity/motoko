// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::types::{Bits64, Value};

use super::{Serializer, StableValue, StaticScanner, STABLE_TAG_BITS64};

#[repr(C)]
#[derive(Default)]
pub struct StableBits64 {
    bits: u64,
}

impl StaticScanner<StableValue> for StableBits64 {}
impl StaticScanner<Value> for Bits64 {}

impl Serializer<Bits64> for StableBits64 {
    fn stable_tag() -> super::StableTag {
        STABLE_TAG_BITS64
    }

    unsafe fn serialize_static_part(main_object: *mut Bits64) -> Self {
        StableBits64 {
            bits: (*main_object).bits(),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Bits64 {
        let bits = stable_object.read_unaligned().bits;
        Bits64::new(target_address, bits)
    }
}
