use crate::{
    stabilization::{stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream}, stable_memory_access::StableMemoryAccess},
    types::{Object, Value, Words, size_of, TAG_OBJECT}, memory::Memory, barriers::allocation_barrier,
};

use super::{Serializer, StableToSpace, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

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
        for index in 0..main_object.size() {
            let main_field = main_object.get(index);
            let stable_field = StableValue::serialize(main_field);
            memory.write(&stable_field);
        }
    }

    fn scan_serialized_dynamic<C: StableToSpace, F: Fn(&mut C, StableValue) -> StableValue>(
        context: &mut C,
        stable_object: &Self,
        translate: &F,
    ) {
        for _ in 0..stable_object.size {
            let old_value = context.to_space().read::<StableValue>();
            // On a longer term, the GC could remove unnecessary fields (during evacuation) that have been
            // declared in old program versions but which name does no longer exist in a new program version.
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
        let stable_object = stable_memory.read::<StableObject>(stable_address);
        let total_size = size_of::<Object>() + Words(stable_object.size);
        let target_object = main_memory.alloc_words(total_size);
        let main_object = target_object.get_ptr() as *mut Object;
        (*main_object).header.tag = TAG_OBJECT;
        (*main_object).header.init_forward(target_object);
        (*main_object).size = stable_object.size;
        (*main_object).hash_blob = stable_object.hash_blob.deserialize();
        for index in 0..stable_object.size {
            let field_address = stable_address + size_of::<StableObject>().to_bytes().as_usize() as u64 +
                (index * size_of::<StableValue>().to_bytes().as_u32()) as u64;
            let field = stable_memory.read::<StableValue>(field_address);
            let target_field_address = main_object.payload_addr().add(index as usize);
            *target_field_address = field.deserialize();
        }
        allocation_barrier(target_object)
    }
}
