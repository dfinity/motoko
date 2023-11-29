// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::types::{Bits64, Value};

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

    unsafe fn deserialize<M: crate::memory::Memory>(
        main_memory: &mut M,
        stable_memory: &crate::stabilization::stable_memory_access::StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        todo!()
    }

    // unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Bits64 {
    //     let bits = stable_object.read_unaligned().bits;
    //     Bits64 {
    //         header: Obj::new(TAG_BITS64, target_address),
    //         bits_lo: lower32(bits),
    //         bits_hi: upper32(bits),
    //     }
    // }
}
