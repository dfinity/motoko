use crate::{
    memory::Memory,
    types::{Value, TAG_NULL, TAG_OBJECT, Obj},
};
use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn(ic_only)]
unsafe fn follow_forwarding_pointer<M: Memory>(_mem: &mut M, value: Value) -> Value {
    const TRUE_VALUE: u32 = 1;
    assert!(value.is_ptr() && value.get_raw() != TRUE_VALUE);
    assert!(value.forward().is_ptr());
    assert!(value.forward().get_ptr() == value.get_ptr());
    assert!(value.tag() >= TAG_OBJECT && value.tag() <= TAG_NULL);
    value.forward()
}
