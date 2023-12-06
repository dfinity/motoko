use crate::types::{Some, Value, TAG_SOME};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableSome {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableSome {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<Some> for StableSome {
    unsafe fn serialize_static_part(main_object: *mut Some) -> Self {
        StableSome {
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize_static_part(&self, target_some: *mut Some) {
        (*target_some).header.tag = TAG_SOME;
        (*target_some)
            .header
            .init_forward(Value::from_ptr(target_some as usize));
        (*target_some).field = self.field.deserialize();
    }
}
