use crate::{
    memory::Memory,
    stabilization::{
        deserialization::stable_memory_access::StableMemoryAccess,
        layout::StableObjectKind,
        serialization::{
            stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
            SerializationContext,
        },
    },
    types::{size_of, NewClosure, Value, Words, TAG_NEW_CLOSURE},
};

use super::{
    stable_hash_blob::size_by_hash_blob, Serializer, StableToSpace, StableValue, StaticScanner,
};

#[repr(C)]
pub struct StableClosure {
    function_id: i64, // Stable function id.
    size: u64,        // Number of captured variables.
    hash_blob: StableValue, // Pointer to a blob containing the `u32` hashes of the captured variable ids.
                            // Dynamically sized body with `size` fields, each of `StableValue` being a captured variable.
}

impl StaticScanner<StableValue> for StableClosure {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.hash_blob = translate(context, self.hash_blob);
        true
    }
}

impl Serializer<NewClosure> for StableClosure {
    unsafe fn serialize_static_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut NewClosure,
    ) -> Self {
        debug_assert!(!is_flexible_function_id((*main_object).funid));
        StableClosure {
            function_id: (*main_object).funid as i64,
            size: get_closure_size(stable_memory, main_object) as u64,
            hash_blob: StableValue::serialize((*main_object).hash_blob),
        }
    }

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut NewClosure,
    ) {
        let closure_size = get_closure_size(stable_memory, main_object);
        for index in 0..closure_size {
            let main_captured = main_object.get(index);
            let stable_captured = StableValue::serialize(main_captured);
            stable_memory.write(&stable_captured);
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
            let new_value = translate(context, old_value);
            context.serialization.to_space().update(&new_value);
        }
    }

    unsafe fn allocate_deserialized<M: Memory>(
        &self,
        main_memory: &mut M,
        object_kind: StableObjectKind,
    ) -> Value {
        debug_assert_eq!(object_kind, StableObjectKind::Closure);
        let total_size = size_of::<NewClosure>() + Words(self.size as usize);
        main_memory.alloc_words(total_size)
    }

    unsafe fn deserialize_static_part(
        &self,
        target_object: *mut NewClosure,
        object_kind: StableObjectKind,
    ) {
        debug_assert_eq!(object_kind, StableObjectKind::Closure);
        debug_assert!(!is_flexible_function_id(self.function_id as isize));
        (*target_object).header.tag = TAG_NEW_CLOSURE;
        (*target_object)
            .header
            .init_forward(Value::from_ptr(target_object as usize));
        (*target_object).funid = self.function_id as isize;
        (*target_object).hash_blob = self.hash_blob.deserialize();
    }

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_object: *mut NewClosure,
    ) {
        let stable_address = stable_object.payload_address();
        for index in 0..self.size {
            let field_address = stable_address
                + size_of::<StableClosure>().to_bytes().as_usize() as u64
                + (index * size_of::<StableValue>().to_bytes().as_usize() as u64);
            let field = stable_memory.read::<StableValue>(field_address);
            let target_field_address = target_object.payload_addr().add(index as usize);
            *target_field_address = field.deserialize();
        }
    }
}

// Wrappers are needed for RTS unit testing.
#[cfg(feature = "ic")]
fn is_flexible_function_id(function_id: isize) -> bool {
    crate::persistence::stable_functions::is_flexible_function_id(function_id)
}

#[cfg(not(feature = "ic"))]
fn is_flexible_function_id(_function_id: isize) -> bool {
    true
}

/// Resolve closure size (number of captured variables) during serialization.
/// This requires a look up in the hash blob, which may however already have been
/// serialized to stable memory.
fn get_closure_size(stable_memory: &StableMemoryStream, main_object: *mut NewClosure) -> usize {
    // Do not call tag as it resolves the forwarding pointer.
    unsafe {
        let main_hash_blob = (*main_object).hash_blob;
        size_by_hash_blob(stable_memory, main_hash_blob)
    }
}
