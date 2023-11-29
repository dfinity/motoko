// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::{types::{Bits64, Value, size_of, TAG_BITS64, lower32, upper32}, barriers::allocation_barrier};

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
        let stable_address = stable_object.payload_address();
        let stable_bits64 = stable_memory.read::<StableBits64>(stable_address);
        let target_object = main_memory.alloc_words(size_of::<Bits64>());
        let target_bits64 = target_object.get_ptr() as *mut Bits64;
        (*target_bits64).header.tag = TAG_BITS64;
        (*target_bits64).header.init_forward(target_object);
        (*target_bits64).bits_lo = lower32(stable_bits64.bits);
        (*target_bits64).bits_hi = upper32(stable_bits64.bits);
        allocation_barrier(target_object)
    }
}
