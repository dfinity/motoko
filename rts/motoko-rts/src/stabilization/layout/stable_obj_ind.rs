// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::{
    stabilization::StableMemoryAccess,
    types::{Obj, ObjInd, Value, TAG_OBJ_IND},
};

use super::{Serializer, StableValue, StaticScanner};

#[repr(C)]
pub struct StableObjInd {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableObjInd {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl StaticScanner<Value> for ObjInd {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<ObjInd> for StableObjInd {
    unsafe fn serialize_static_part(main_object: *mut ObjInd) -> Self {
        StableObjInd {
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> ObjInd {
        let field = stable_object.read_unaligned().field.deserialize();
        ObjInd {
            header: Obj::new(TAG_OBJ_IND, target_address),
            field,
        }
    }
}
