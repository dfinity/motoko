use crate::{
    stabilization::StableMemoryAccess,
    types::{Region, Value},
};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
#[derive(Default)]
pub struct StableRegion {
    id: u64,
    page_count: u32,
    vec_pages: StableValue, // Blob of `u16`, each denoting a page ID.
}

impl StaticScanner<StableValue> for StableRegion {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.vec_pages = translate(context, self.vec_pages);
        true
    }
}

impl StaticScanner<Value> for Region {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.vec_pages = translate(context, self.vec_pages);
        true
    }
}

impl Serializer<Region> for StableRegion {
    unsafe fn serialize_static_part(main_object: *mut Region) -> Self {
        StableRegion {
            id: main_object.read_id64(),
            page_count: main_object.read_unaligned().page_count,
            vec_pages: StableValue::serialize(main_object.read_unaligned().vec_pages),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Region {
        let vec_pages = stable_object.read_unaligned().vec_pages.deserialize();
        Region::new(
            target_address,
            stable_object.read_unaligned().id,
            stable_object.read_unaligned().page_count,
            vec_pages,
        )
    }
}
