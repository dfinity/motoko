use crate::types::{Value, Variant};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
pub struct StableVariant {
    tag: u32,
    field: StableValue,
}

impl StaticScanner<StableValue> for StableVariant {
    fn update_pointers<
        C,
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

impl Serializer<Variant> for StableVariant {
    unsafe fn serialize_static_part(main_object: *mut Variant) -> Self {
        StableVariant {
            tag: (*main_object).tag,
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize<M: crate::memory::Memory>(
        main_memory: &mut M,
        stable_memory: &crate::stabilization::stable_memory_access::StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        todo!()
    }

    // unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Variant {
    //     let tag = stable_object.read_unaligned().tag;
    //     let field = stable_object.read_unaligned().field.deserialize();
    //     Variant {
    //         header: Obj::new(TAG_VARIANT, target_address),
    //         tag,
    //         field,
    //     }
    // }
}
