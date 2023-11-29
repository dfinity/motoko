use crate::{
    stabilization::StableMemoryAccess,
    types::{MutBox, Value, size_of, TAG_MUTBOX}, barriers::allocation_barrier,
};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
pub struct StableMutBox {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableMutBox {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<MutBox> for StableMutBox {
    unsafe fn serialize_static_part(main_object: *mut MutBox) -> Self {
        StableMutBox {
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize<M: crate::memory::Memory>(
        main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let address = stable_object.payload_address();
        let stable_mutbox = stable_memory.read::<StableMutBox>(address);
        let target_object = main_memory.alloc_words(size_of::<MutBox>());
        let target_mutbox = target_object.get_ptr() as *mut MutBox;
        (*target_mutbox).header.tag = TAG_MUTBOX;
        (*target_mutbox).header.init_forward(target_object);
        (*target_mutbox).field = stable_mutbox.field.deserialize();
        allocation_barrier(target_object)
    }
}
