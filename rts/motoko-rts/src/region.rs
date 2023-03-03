use crate::memory::{ic::NEXT_REGION_ID, Memory, alloc_blob};
use crate::types::{size_of, Region, Value, Words, Blob, Bytes, TAG_REGION};
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
    (*region).vec_pages = alloc_blob(mem, Bytes(0));
    Value::from_ptr(region as usize)
}

#[ic_mem_fn]
pub unsafe fn region_id<M: Memory>(_mem: &mut M, r: Value) -> u32 {
    let r = r.as_region();
    (*r).id.into()
}

#[ic_mem_fn]
pub unsafe fn region_size<M: Memory>(_mem: &mut M, r: Value) -> u64 {
    let r = r.as_region();
    (*r).page_count.into()
}

#[ic_mem_fn]
pub unsafe fn region_grow<M: Memory>(mem: &mut M, r: Value, new_pages: u64) -> u64 {
    let r = r.as_region();
    let new_pages_ = new_pages as u32;
    let old_page_count = (*r).page_count;
    let new_block_count = (old_page_count + new_pages_) / 128;
    (*r).page_count += new_pages_;
    let new_vec_pages = alloc_blob(mem, Bytes(new_block_count * 2));
    // ## copy old block IDs:
    //  - number of valid entries in region before growth:
    //     = old_page_count + 127 / 128
    //  - two bytes per entry.
    for i in 0..(old_page_count + 127 / 128) * 2 {
	let new_pages = new_vec_pages.get_ptr() as *mut Blob;
	let old_pages = (*r).vec_pages.get_ptr() as *mut Blob;
	new_pages.set(i, old_pages.get(i));
    }
    // ## choose and record new block IDs:
    //  TO DO
    (*r).vec_pages = new_vec_pages;
    old_page_count.into()
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
