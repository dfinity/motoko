use crate::{
    stabilization::StableMemoryAccess,
    types::{lower32, upper32, Obj, Region, Value, TAG_REGION},
};

use super::{Serializer, StableValue, StaticScanner};

// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

#[repr(C)]
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
        let id = stable_object.read_unaligned().id;
        let page_count = stable_object.read_unaligned().page_count;
        let vec_pages = stable_object.read_unaligned().vec_pages.deserialize();
        Region {
            header: Obj::new(TAG_REGION, target_address),
            id_lower: lower32(id),
            id_upper: upper32(id),
            page_count,
            vec_pages,
        }
    }
}
