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
    types::{size_of, Closure, Value, Words, TAG_CLOSURE},
};

use super::{Serializer, StableToSpace, StableValue, StaticScanner};

#[repr(C)]
pub struct StableClosure {
    function_id: i64, // Stable function id.
    size: u64,        // Number of fields.
                      // Dynamically sized body with `size` fields, each of `StableValue` being a captured variable.
}

impl StaticScanner<StableValue> for StableClosure {}

impl Serializer<Closure> for StableClosure {
    unsafe fn serialize_static_part(
        _stable_memory: &mut StableMemoryStream,
        main_object: *mut Closure,
    ) -> Self {
        debug_assert!(!is_flexible_function_id((*main_object).funid));
        StableClosure {
            function_id: (*main_object).funid as i64,
            size: (*main_object).size as u64,
        }
    }

    unsafe fn serialize_dynamic_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut Closure,
    ) {
        for index in 0..(*main_object).size {
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
        let total_size = size_of::<Closure>() + Words(self.size as usize);
        main_memory.alloc_words(total_size)
    }

    unsafe fn deserialize_static_part(
        &self,
        target_object: *mut Closure,
        object_kind: StableObjectKind,
    ) {
        debug_assert_eq!(object_kind, StableObjectKind::Closure);
        debug_assert!(!is_flexible_function_id(self.function_id as isize));
        (*target_object).header.tag = TAG_CLOSURE;
        (*target_object)
            .header
            .init_forward(Value::from_ptr(target_object as usize));
        (*target_object).size = self.size as usize;
        (*target_object).funid = self.function_id as isize;
    }

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
        target_object: *mut Closure,
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
