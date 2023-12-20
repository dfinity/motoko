use crate::{
    memory::{alloc_array, Memory},
    stabilization::{
        stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
        StableMemoryAccess,
    },
    types::{size_of, Array, Value, TAG_ARRAY},
};

use super::{checked_to_u32, Serializer, StableToSpace, StableValue, StaticScanner};

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

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_array: *mut Array,
    ) {
        for index in 0..main_array.len() {
            let main_element = main_array.get(index);
            let stable_element = StableValue::serialize(main_element);
            stable_memory.write(&stable_element);
        }
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        translate: &F,
    ) {
        for _ in 0..self.array_length {
            let old_value = context.to_space().read::<StableValue>();
            let new_value = translate(context, old_value);
            context.to_space().update(&new_value);
        }
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let array_length = checked_to_u32(self.array_length);
        alloc_array(main_memory, array_length)
    }

    unsafe fn deserialize_static_part(&self, target_array: *mut Array) {
        debug_assert_eq!((*target_array).header.tag, TAG_ARRAY);
        debug_assert_eq!((*target_array).len, checked_to_u32(self.array_length));
    }

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_array: *mut Array,
    ) {
        let stable_address = stable_object.payload_address();
        let mut element_address =
            stable_address + size_of::<StableArray>().to_bytes().as_usize() as u64;
        for index in 0..(*target_array).len {
            let element = stable_memory.read::<StableValue>(element_address);
            target_array.set_raw(index, element.deserialize());
            element_address += size_of::<StableValue>().to_bytes().as_u32() as u64;
        }
    }
}
