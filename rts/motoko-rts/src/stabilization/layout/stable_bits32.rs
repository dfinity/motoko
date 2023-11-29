use crate::{types::{Bits32, Value, size_of, TAG_BITS32}, barriers::allocation_barrier};

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

    unsafe fn deserialize<M: crate::memory::Memory>(
        main_memory: &mut M,
        stable_memory: &crate::stabilization::stable_memory_access::StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let stable_address = stable_object.payload_address();
        let stable_bits32 = stable_memory.read::<StableBits32>(stable_address);
        let target_object = main_memory.alloc_words(size_of::<Bits32>());
        let target_bits32 = target_object.get_ptr() as *mut Bits32;
        (*target_bits32).header.tag = TAG_BITS32;
        (*target_bits32).header.init_forward(target_object);
        (*target_bits32).bits = stable_bits32.bits;
        allocation_barrier(target_object)
    }
}
