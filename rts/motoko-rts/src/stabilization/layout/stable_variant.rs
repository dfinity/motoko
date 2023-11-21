use crate::types::{Value, Variant};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
#[derive(Default)]
pub struct StableVariant {
    tag: u32,
    field: StableValue,
}

impl StaticScanner<StableValue> for StableVariant {
    fn update_pointers<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, StableValue) -> StableValue,
    >(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl StaticScanner<Value> for Variant {
    fn update_pointers<
        C: crate::stabilization::StableMemoryAccess,
        F: Fn(&mut C, Value) -> Value,
    >(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<Variant> for StableVariant {
    unsafe fn serialize_static_part(main_object: *mut Variant) -> Self {
        StableVariant {
            tag: (*main_object).tag,
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Variant {
        let field = stable_object.read_unaligned().field.deserialize();
        Variant::new(target_address, stable_object.read_unaligned().tag, field)
    }
}
