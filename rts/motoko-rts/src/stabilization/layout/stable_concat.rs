// Note: The unaligned reads are needed because heap allocations are aligned to 32-bit,
// while the stable layout uses 64-bit values.

use crate::{
    stabilization::StableMemoryAccess,
    types::{Bytes, Concat, Value},
};

use super::{checked_to_u32, Serializer, StableValue, StaticScanner};

#[repr(C)]
#[derive(Default)]
pub struct StableConcat {
    number_of_bytes: u64,
    text1: StableValue,
    text2: StableValue,
}

impl StaticScanner<StableValue> for StableConcat {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.text1 = translate(context, self.text1);
        self.text2 = translate(context, self.text2);
        true
    }
}

impl StaticScanner<Value> for Concat {
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        &mut self,
        context: &mut C,
        translate: &F,
    ) -> bool {
        self.text1 = translate(context, self.text1);
        self.text2 = translate(context, self.text2);
        true
    }
}

impl Serializer<Concat> for StableConcat {
    unsafe fn serialize_static_part(main_object: *mut Concat) -> Self {
        StableConcat {
            number_of_bytes: (*main_object).n_bytes.as_u32() as u64,
            text1: StableValue::serialize(main_object.text1()),
            text2: StableValue::serialize(main_object.text2()),
        }
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> Concat {
        let number_of_bytes = Bytes(checked_to_u32(
            stable_object.read_unaligned().number_of_bytes,
        ));
        let text1 = stable_object.read_unaligned().text1.deserialize();
        let text2 = stable_object.read_unaligned().text2.deserialize();
        Concat::new(target_address, number_of_bytes, text1, text2)
    }
}
