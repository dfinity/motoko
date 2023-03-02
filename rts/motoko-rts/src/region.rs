use crate::memory::{ic::NEXT_REGION_ID, Memory};
use crate::types::{size_of, Region, Value, Words, TAG_REGION};
use crate::rts_trap_with;

use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn]
pub unsafe fn region_new<M: Memory>(mem: &mut M) -> Value {
    let r_ptr = mem.alloc_words(size_of::<Region>() + Words(1));
    // NB. cannot use as_region() here as we didn't write the header yet
    let region = r_ptr.get_ptr() as *mut Region;
    (*region).header.tag = TAG_REGION;
    (*region).id = NEXT_REGION_ID;
    NEXT_REGION_ID += 1;
    (*region).page_count = 0;
    Value::from_ptr(region as usize)
}

#[ic_mem_fn]
pub unsafe fn region_id<M: Memory>(_mem: &mut M, r: Value) -> Value {
    let r = r.as_region();
    Value::from_raw((*r).id as u32) // is this right?  It makes the test pass.
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
    Value::from_scalar(NEXT_REGION_ID as u32)
}
