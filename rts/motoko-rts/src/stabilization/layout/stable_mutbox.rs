use crate::{
    stabilization::StableMemoryAccess,
    types::{MutBox, Value},
};

use super::{Serializer, StableValue, StaticScanner, STABLE_TAG_MUTBOX};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
#[derive(Default)]
pub struct StableMutBox {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableMutBox {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl StaticScanner<Value> for MutBox {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<MutBox> for StableMutBox {
    fn stable_tag() -> super::StableTag {
        STABLE_TAG_MUTBOX
    }

    unsafe fn serialize_static_part(main_object: *mut MutBox) -> Self {
        StableMutBox {
            field: StableValue::serialize(main_object.read_unaligned().field),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> MutBox {
        let field = stable_object.read_unaligned().field.deserialize();
        MutBox::new(target_address, field)
    }
}
