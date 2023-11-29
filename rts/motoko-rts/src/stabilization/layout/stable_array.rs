use crate::{
    barriers::allocation_barrier,
    memory::{alloc_array, Memory},
    stabilization::{
        stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
        StableMemoryAccess,
    },
    types::{size_of, Array, Value},
};

use super::{checked_to_u32, Serializer, StableToSpace, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
pub struct StableArray {
    array_length: u64,
    // Dynamically sized body with `array_length` elements, each of `StableValue`.
}

impl StaticScanner<StableValue> for StableArray {}

impl Serializer<Array> for StableArray {
    unsafe fn serialize_static_part(array: *mut Array) -> Self {
        StableArray {
            array_length: array.len() as u64,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemoryStream, main_array: *mut Array) {
        for index in 0..main_array.len() {
            let main_element = main_array.get(index);
            let stable_element = StableValue::serialize(main_element);
            memory.write(&stable_element);
        }
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        context: &mut C,
        stable_array: &Self,
        translate: &F,
    ) {
        for _ in 0..stable_array.array_length {
            let old_value = context.to_space().read::<StableValue>();
            let new_value = translate(context, old_value);
            context.to_space().update(&new_value);
        }
    }

    unsafe fn deserialize<M: Memory>(
        main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let stable_address = stable_object.payload_address();
        let stable_array = stable_memory.read::<StableArray>(stable_address);
        let array_length = checked_to_u32(stable_array.array_length);
        let target_object = alloc_array(main_memory, array_length);
        let target_array = target_object.as_array();
        for index in 0..array_length {
            let element_address = stable_address
                + size_of::<StableArray>().to_bytes().as_usize() as u64
                + (index * size_of::<StableValue>().to_bytes().as_u32()) as u64;
            let element = stable_memory.read::<StableValue>(element_address);
            target_array.set_raw(index, element.deserialize());
        }
        allocation_barrier(target_object)
    }
}
