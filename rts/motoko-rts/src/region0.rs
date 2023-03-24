//use crate::region::Region;
use crate::memory::Memory;
use crate::region::{region_grow, region_size};
use crate::types::{Blob, Value};

use motoko_rts_macros::ic_mem_fn;

unsafe fn region0_load<M: Memory>(mem: &mut M, offset: u64, dst: &mut [u8]) {
    let r = crate::memory::ic::REGION_0;
    crate::region::region_load(mem, r, offset, dst)
}

unsafe fn region0_store<M: Memory>(mem: &mut M, offset: u64, src: &[u8]) {
    let r = crate::memory::ic::REGION_0;
    crate::region::region_store(mem, r, offset, src)
}

#[ic_mem_fn]
pub unsafe fn region0_get<M: Memory>(_mem: &mut M) -> Value {
    let v = crate::memory::ic::REGION_0;
    if false {
        println!(80, "region0_get() ~> {:?}", v);
    }
    v
}

#[cfg(feature = "ic")]
pub(crate) unsafe fn region0_get_ptr_loc() -> *mut Value {
    &mut crate::memory::ic::REGION_0
}

#[ic_mem_fn]
pub unsafe fn region0_size<M: Memory>(mem: &mut M) -> u64 {
    region_size(mem, crate::memory::ic::REGION_0)
}

#[ic_mem_fn]
pub unsafe fn region0_grow<M: Memory>(mem: &mut M, new_pages: u64) -> u64 {
    region_grow(mem, crate::memory::ic::REGION_0, new_pages)
}

// -- Region0 load operations.

#[ic_mem_fn]
pub unsafe fn region0_load_word8<M: Memory>(mem: &mut M, offset: u64) -> u32 {
    let mut byte: [u8; 1] = [0];
    region0_load(mem, offset, &mut byte);
    byte[0] as u32
}

#[ic_mem_fn]
pub unsafe fn region0_load_word16<M: Memory>(mem: &mut M, offset: u64) -> u32 {
    let mut bytes: [u8; 2] = [0; 2];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u16::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region0_load_word32<M: Memory>(mem: &mut M, offset: u64) -> u32 {
    let mut bytes: [u8; 4] = [0; 4];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u32::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region0_load_word64<M: Memory>(mem: &mut M, offset: u64) -> u64 {
    let mut bytes: [u8; 8] = [0; 8];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u64::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region0_load_float64<M: Memory>(mem: &mut M, offset: u64) -> f64 {
    let mut bytes: [u8; 8] = [0; 8];
    region0_load(mem, offset, &mut bytes);
    core::primitive::f64::from_le_bytes(bytes).into()
}

// -- Region0 store operations.

#[ic_mem_fn]
pub unsafe fn region0_store_word8<M: Memory>(mem: &mut M, offset: u64, byte: u32) {
    let mut byte: [u8; 1] = [byte as u8];
    region0_store(mem, offset, &mut byte);
}

#[ic_mem_fn]
pub unsafe fn region0_store_word16<M: Memory>(mem: &mut M, offset: u64, val: u32) {
    region0_store(mem, offset, &core::primitive::u16::to_le_bytes(val as u16))
}

#[ic_mem_fn]
pub unsafe fn region0_store_word32<M: Memory>(mem: &mut M, offset: u64, val: u32) {
    region0_store(mem, offset, &core::primitive::u32::to_le_bytes(val as u32))
}

#[ic_mem_fn]
pub unsafe fn region0_store_word64<M: Memory>(mem: &mut M, offset: u64, val: u64) {
    region0_store(mem, offset, &core::primitive::u64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_float64<M: Memory>(mem: &mut M, offset: u64, val: f64) {
    region0_store(mem, offset, &core::primitive::f64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_blob<M: Memory>(mem: &mut M, offset: u64, blob: Value) {
    let blob: *const Blob = blob.as_blob();
    let len = blob.len();
    let bytes = blob.payload_const();
    let bytes: &[u8] = core::slice::from_raw_parts(bytes, len.0 as usize);
    //println!(80, "store_blob offset={:?} blob={:?}", offset, blob);
    region0_store(mem, offset, bytes)
}

#[ic_mem_fn]
pub unsafe fn region0_load_blob<M: Memory>(mem: &mut M, offset: u64, len: u32) -> Value {
    let blob_val = crate::memory::alloc_blob(mem, crate::types::Bytes(len));
    let blob = blob_val.as_blob_mut();
    let bytes: &mut [u8] = core::slice::from_raw_parts_mut(blob.payload_addr(), len as usize);
    //println!(80, "load_blob offset={:?} len={:?} ~~> Blob {:?}, with bytes @ {:?}", offset, len, blob_val, bytes as *const [u8]);
    region0_load(mem, offset, bytes);
    blob_val
}
