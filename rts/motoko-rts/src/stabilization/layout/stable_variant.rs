use crate::types::{Value, Variant, TAG_VARIANT};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableVariant {
    tag: u32,
    field: StableValue,
}

impl StaticScanner<StableValue> for StableVariant {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
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

    unsafe fn deserialize_static_part(&self, target_variant: *mut Variant) {
        (*target_variant).header.tag = TAG_VARIANT;
        (*target_variant)
            .header
            .init_forward(Value::from_ptr(target_variant as usize));
        (*target_variant).tag = self.tag;
        (*target_variant).field = self.field.deserialize();
    }
}
