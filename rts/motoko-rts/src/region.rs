use crate::memory::{ic::NEXT_REGION_ID, Memory};
use crate::rts_trap_with;
use crate::types::{Value};

use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn]
pub unsafe fn region_new<M: Memory>(_mem: &mut M) -> Value {
    rts_trap_with("TODO region_new");
}

#[ic_mem_fn]
pub unsafe fn region_size<M: Memory>(_mem: &mut M, _r: Value) -> Value {
    rts_trap_with("TODO region_grow");
}

#[ic_mem_fn]
pub unsafe fn region_grow<M: Memory>(_mem: &mut M, _r: Value, _new_pages: Value) -> Value {
    rts_trap_with("TODO region_grow");
}

#[ic_mem_fn]
pub unsafe fn region_load_blob<M: Memory>(_mem: &mut M, _r: Value, _start: Value, _len: Value) -> Value {
    rts_trap_with("TODO region_load_blob");
}

#[ic_mem_fn]
pub unsafe fn region_store_blob<M: Memory>(_mem: &mut M, _r: Value, _start: Value, _blob: Value) {
    rts_trap_with("TODO region_store_blob");
}


#[ic_mem_fn]
pub unsafe fn region_next_id<M: Memory>(_mem: &mut M) -> Value {
    Value::from_scalar(NEXT_REGION_ID)
}
