use crate::types::{lower32, upper32, Region, Value, TAG_REGION};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableRegion {
    id: u64,
    page_count: u32,
    vec_pages: StableValue, // Blob of `u16`, each denoting a page ID.
}

impl StaticScanner<StableValue> for StableRegion {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
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

    unsafe fn deserialize_static_part(&self, target_region: *mut Region) {
        (*target_region).header.tag = TAG_REGION;
        (*target_region)
            .header
            .init_forward(Value::from_ptr(target_region as usize));
        (*target_region).id_lower = lower32(self.id);
        (*target_region).id_upper = upper32(self.id);
        (*target_region).page_count = self.page_count;
        (*target_region).vec_pages = self.vec_pages.deserialize();
    }
}
