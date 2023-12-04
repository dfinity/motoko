use crate::{
    memory::{alloc_array, Memory},
    stabilization::{
        stable_memory_access::read_series,
        stable_memory_stream::{update_series, write_series, StableMemoryStream},
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

    unsafe fn serialize_dynamic_part(memory: &mut StableMemoryStream, main_array: *mut Array) {
        write_series(memory, main_array.len() as u64, &|index| {
            StableValue::serialize(main_array.get(index as u32))
        });
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        translate: &F,
    ) {
        update_series(context, self.array_length, translate);
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let array_length = checked_to_u32(self.array_length);
        alloc_array(main_memory, array_length)
    }

    unsafe fn deserialize_static_part(&self, target_array: *mut Array) {
        debug_assert_eq!((*target_array).header.tag, TAG_ARRAY);
        debug_assert_eq!((*target_array).len, checked_to_u32(self.array_length));
    }

    unsafe fn deserialize_dynamic_part(
        &self,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_array: *mut Array,
    ) {
        let stable_address = stable_object.payload_address();
        let source_address = stable_address + size_of::<StableArray>().to_bytes().as_usize() as u64;
        read_series(
            stable_memory,
            source_address,
            (*target_array).len as u64,
            &|index, element: StableValue| {
                target_array.set_raw(index as u32, element.deserialize());
            },
        );
    }
}
