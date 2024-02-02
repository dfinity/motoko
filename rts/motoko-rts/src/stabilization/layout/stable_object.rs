use crate::{
    memory::Memory,
    stabilization::{
        deserialization::stable_memory_access::StableMemoryAccess,
        serialization::{
            stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
            SerializationContext,
        },
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

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut Object,
    ) {
        for index in 0..main_object.size() {
            let main_field = main_object.get(index);
            let stable_field = StableValue::serialize(main_field);
            stable_memory.write(&stable_field);
        }
    }

    fn scan_serialized_dynamic<
        'a,
        M,
        F: Fn(&mut SerializationContext<'a, M>, StableValue) -> StableValue,
    >(
        &self,
        context: &mut SerializationContext<'a, M>,
        translate: &F,
    ) {
        for _ in 0..self.size {
            let old_value = context.serialization.to_space().read::<StableValue>();
            // On a longer term, the GC could remove unnecessary fields (during evacuation) that have been
            // declared in old program versions but which name does no longer exist in a new program version.
            let new_value = translate(context, old_value);
            context.serialization.to_space().update(&new_value);
        }
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

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_object: *mut Object,
    ) {
        let stable_address = stable_object.payload_address();
        for index in 0..self.size {
            let field_address = stable_address
                + size_of::<StableObject>().to_bytes().as_usize() as u64
                + (index * size_of::<StableValue>().to_bytes().as_u32()) as u64;
            let field = stable_memory.read::<StableValue>(field_address);
            let target_field_address = target_object.payload_addr().add(index as usize);
            *target_field_address = field.deserialize();
        }
    }
}
