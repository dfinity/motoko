use crate::{
    stabilization::{
        layout::checked_to_usize,
        reader_writer::{ScanStream, StableMemorySpace, WriteStream},
        StableMemoryAccess,
    },
    types::{Array, Value},
};

use super::{checked_to_u32, Serializer, StableTag, StableValue, StaticScanner, STABLE_TAG_ARRAY};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
#[derive(Default)]
pub struct StableArray {
    array_length: u64,
    // Dynamically sized body with `array_length` elements, each of `StableValue`.
}

impl StableArray {
    unsafe fn array_length(self: *const Self) -> u64 {
        self.read_unaligned().array_length
    }

    unsafe fn elements(self: *const Self) -> *const StableValue {
        self.offset(1) as *const StableValue
    }

    unsafe fn get(self: *const Self, index: u64) -> StableValue {
        debug_assert!(index < self.array_length());
        self.elements()
            .add(checked_to_usize(index))
            .read_unaligned()
    }
}

impl StaticScanner<StableValue> for StableArray {}

impl StaticScanner<Value> for Array {}

impl Serializer<Array> for StableArray {
    fn stable_tag() -> StableTag {
        STABLE_TAG_ARRAY
    }

    unsafe fn serialize_static_part(array: *mut Array) -> Self {
        StableArray {
            array_length: array.len() as u64,
        }
    }

    unsafe fn serialize_dynamic_part(memory: &mut StableMemorySpace, main_array: *mut Array) {
        for index in 0..main_array.len() {
            let main_element = main_array.get(index);
            let stable_element = StableValue::serialize(main_element);
            memory.write(&stable_element);
        }
    }

    fn scan_serialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
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

    unsafe fn deserialize_static_part(stable_array: *mut Self, target_address: Value) -> Array {
        let length = checked_to_u32(stable_array.array_length());
        Array::new(target_address, length)
    }

    unsafe fn deserialize_dynamic_part(memory: &mut StableMemorySpace, stable_array: *mut Self) {
        for index in 0..stable_array.array_length() {
            let stable_element = stable_array.get(index);
            let main_element = stable_element.deserialize();
            memory.write(&main_element);
        }
    }

    fn scan_deserialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
        main_object: &Array,
        translate: &F,
    ) {
        for _ in 0..main_object.len {
            let old_value = context.to_space().read::<Value>();
            let new_value = translate(context, old_value);
            context.to_space().update(&new_value);
        }
    }
}
