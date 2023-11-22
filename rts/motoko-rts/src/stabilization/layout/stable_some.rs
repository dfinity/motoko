// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::{
    stabilization::StableMemoryAccess,
    types::{Obj, Some, Value, TAG_SOME},
};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableSome {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableSome {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl StaticScanner<Value> for Some {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
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

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Some {
        let field = stable_object.read_unaligned().field.deserialize();
        Some {
            header: Obj::new(TAG_SOME, target_address),
            field,
        }
    }
}
