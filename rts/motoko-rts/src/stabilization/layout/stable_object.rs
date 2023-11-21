use crate::{
    stabilization::reader_writer::{ScanStream, StableMemorySpace, WriteStream},
    types::{Obj, Object, Value, TAG_OBJECT},
};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
pub struct StableObject {
    size: u32, // Number of fields.
    hash_blob: StableValue, // Pointer to a blob containing the `u32` hashes of the field labels.
               // Dynamically sized body with `size` fields, each of `StableValue`, ordered according to the hashes in the blob.
}

impl StableObject {
    unsafe fn size(self: *const Self) -> u32 {
        self.read_unaligned().size
    }

    unsafe fn fields(self: *const Self) -> *const StableValue {
        self.offset(1) as *const StableValue
    }

    unsafe fn get(self: *const Self, index: u32) -> StableValue {
        debug_assert!(index < self.size());
        self.fields().add(index as usize).read_unaligned()
    }
}

impl StaticScanner<StableValue> for StableObject {
    fn update_pointers<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, StableValue) -> StableValue,
    >(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.hash_blob = translate(context, self.hash_blob);
        true
    }
}

impl StaticScanner<Value> for Object {
    fn update_pointers<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, Value) -> Value,
    >(
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

    unsafe fn serialize_dynamic_part(memory: &mut StableMemorySpace, main_object: *mut Object) {
        for index in 0..main_object.size() {
            let main_field = main_object.get(index);
            let stable_field = StableValue::serialize(main_field);
            memory.write(&stable_field);
        }
    }

    fn scan_serialized_dynamic<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, StableValue) -> StableValue,
    >(
        context: &mut C,
        stable_object: &Self,
        translate: &F,
    ) {
        for _ in 0..stable_object.size {
            let old_value = context.to_space().read::<StableValue>();
            let new_value = translate(context, old_value);
            context.to_space().update(&new_value);
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Object {
        let size = stable_object.read_unaligned().size;
        let hash_blob = stable_object.read_unaligned().hash_blob.deserialize();
        Object {
            header: Obj::new(TAG_OBJECT, target_address),
            size,
            hash_blob,
        }
    }

    unsafe fn deserialize_dynamic_part(memory: &mut StableMemorySpace, stable_object: *mut Self) {
        for index in 0..stable_object.size() {
            let stable_field = stable_object.get(index);
            let main_field = stable_field.deserialize();
            memory.write(&main_field);
        }
    }

    fn scan_deserialized_dynamic<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, Value) -> Value,
    >(
        context: &mut C,
        main_object: &Object,
        translate: &F,
    ) {
        for _ in 0..main_object.size {
            let old_value = context.to_space().read::<Value>();
            let new_value = translate(context, old_value);
            context.to_space().update(&new_value);
        }
    }
}