use crate::{
    stabilization::serialization::stable_memory_stream::StableMemoryStream,
    types::{Value, Closure, TAG_CLOSURE},
};

use super::{Serializer, StableObjectKind, StableValue, StaticScanner};

#[repr(C)]
pub struct StableClosure {
    funid: u64,
    size: u64,
    hash: StableValue,
}

impl StaticScanner<StableValue> for StableClosure {
    fn update_pointers<C, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.hash = translate(context, self.hash);
        true
    }
}

impl Serializer<Closure> for StableClosure {
    unsafe fn serialize_static_part(
        _stable_memory: &mut StableMemoryStream,
        main_object: *mut Closure,
    ) -> Self {
        debug_assert_eq!(main_object.funid(), usize::MAX);
        debug_assert_eq!(main_object.size(), 1);
        StableClosure {
	    funid: (*main_object).funid as u64,
            size: (*main_object).size as u64,
            hash: StableValue::serialize(*(main_object.payload_addr())),
        }
    }

    unsafe fn deserialize_static_part(
        &self,
        target_closure: *mut Closure,
        object_kind: StableObjectKind,
    ) {
        debug_assert_eq!(object_kind, StableObjectKind::Closure);
        debug_assert_eq!(self.funid, u64::MAX);
        debug_assert_eq!(self.size, 1);
        (*target_closure).header.tag = TAG_CLOSURE;
        (*target_closure)
            .header
            .init_forward(Value::from_ptr(target_closure as usize));
        (*target_closure).funid = self.funid as usize;
        (*target_closure).size = self.size as usize;
	let address = target_closure.payload_addr();
        *address = self.hash.deserialize();
    }
}
