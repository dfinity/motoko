use crate::{
    memory::Memory,
    stabilization::{
        stable_memory_access::{read_series, StableMemoryAccess},
        stable_memory_stream::{update_series, write_series, StableMemoryStream},
    },
    types::{size_of, Object, Value, Words, TAG_OBJECT},
};

use super::{Serializer, StableToSpace, StableValue, StaticScanner};

#[repr(C)]
pub struct StableObject {
    size: u32, // Number of fields.
    hash_blob: StableValue, // Pointer to a blob containing the `u32` hashes of the field labels.
               // Dynamically sized body with `size` fields, each of `StableValue`, ordered according to the hashes in the blob.
}

impl StaticScanner<StableValue> for StableObject {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.hash_blob = translate(context, self.hash_blob);
        true
    }
}

impl Serializer<Object> for StableObject {
    unsafe fn serialize_static_part(main_object: *mut Object) -> Self {
        StableObject {
            size: (*main_object).size,
            hash_blob: StableValue::serialize((*main_object).hash_blob),
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemoryStream, main_object: *mut Object) {
        write_series(memory, main_object.size() as u64, &|index| {
            StableValue::serialize(main_object.get(index as u32))
        });
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        &self,
        context: &mut C,
        translate: &F,
    ) {
        // On a longer term, the GC could remove unnecessary fields (during evacuation) that have been
        // declared in old program versions but which name does no longer exist in a new program version.
        update_series(context, self.size as u64, translate);
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        let total_size = size_of::<Object>() + Words(self.size);
        main_memory.alloc_words(total_size)
    }

    unsafe fn deserialize_static_part(&self, target_object: *mut Object) {
        (*target_object).header.tag = TAG_OBJECT;
        (*target_object)
            .header
            .init_forward(Value::from_ptr(target_object as usize));
        (*target_object).size = self.size;
        (*target_object).hash_blob = self.hash_blob.deserialize();
    }

    unsafe fn deserialize_dynamic_part(
        &self,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_object: *mut Object,
    ) {
        let stable_address = stable_object.payload_address();
        let source_address =
            stable_address + size_of::<StableObject>().to_bytes().as_usize() as u64;
        read_series(
            stable_memory,
            source_address,
            self.size as u64,
            &|index, field: StableValue| {
                let target_field_address = target_object.payload_addr().add(index as usize);
                *target_field_address = field.deserialize();
            },
        );
    }
}
