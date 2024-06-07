use crate::{
    stabilization::serialization::stable_memory_stream::StableMemoryStream,
    types::{ObjInd, Value, TAG_OBJ_IND},
};

use super::{Serializer, StableObjectKind, StableValue, StaticScanner};

#[repr(C)]
pub struct StableObjInd {
    field: StableValue,
}

impl StaticScanner<StableValue> for StableObjInd {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.field = translate(context, self.field);
        true
    }
}

impl Serializer<ObjInd> for StableObjInd {
    unsafe fn serialize_static_part(
        _stable_memory: &mut StableMemoryStream,
        main_object: *mut ObjInd,
    ) -> Self {
        StableObjInd {
            field: StableValue::serialize((*main_object).field),
        }
    }

    unsafe fn deserialize_static_part(
        &self,
        target_obj_ind: *mut ObjInd,
        object_kind: StableObjectKind,
    ) {
        debug_assert_eq!(object_kind, StableObjectKind::ObjInd);
        (*target_obj_ind).header.tag = TAG_OBJ_IND;
        (*target_obj_ind)
            .header
            .init_forward(Value::from_ptr(target_obj_ind as usize));
        (*target_obj_ind).field = self.field.deserialize();
    }
}
