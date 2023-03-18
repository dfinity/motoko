//use crate::region::Region;
use crate::memory::{Memory};
use crate::types::{Value, Blob};
use crate::region::{RegionObject, region_grow, region_size};

use motoko_rts_macros::ic_mem_fn;

unsafe fn region0_load<M: Memory>(_mem: &mut M, offset:u64, dst: &mut [u8]) {
    let r = RegionObject(crate::memory::ic::REGION_0);
    let abs_off = r.relative_into_absolute_offset(offset);
    // second bounds check on region:
    if dst.len() > 1 {
	let _ = r.relative_into_absolute_offset(offset + dst.len() as u64 - 1);
    };
    crate::ic0_stable::nicer::read(abs_off, dst);
}

unsafe fn region0_store<M: Memory>(_mem: &mut M, offset:u64, src: &[u8]) {
    let r = RegionObject(crate::memory::ic::REGION_0);
    let abs_off = r.relative_into_absolute_offset(offset);
    // second bounds check on region:
    if src.len() > 1 {
	let _ = r.relative_into_absolute_offset(offset + src.len() as u64 - 1);
    };
    crate::ic0_stable::nicer::write(abs_off, src);
}

#[ic_mem_fn]
pub unsafe fn region0_size<M: Memory>(mem: &mut M) -> u64 {
    region_size(mem, Value::from_ptr(crate::memory::ic::REGION_0 as usize))
}

#[ic_mem_fn]
pub unsafe fn region0_grow<M: Memory>(mem: &mut M, new_pages: u64) -> u64 {
    region_grow(mem, Value::from_ptr(crate::memory::ic::REGION_0 as usize), new_pages)
}

// -- Region0 load operations.

#[ic_mem_fn]
pub unsafe fn region0_load_nat8<M: Memory>(mem: &mut M, offset:u64) -> u32 {
    let mut byte : [u8; 1] = [0];
    region0_load(mem, offset, &mut byte);
    byte[0] as u32
}

#[ic_mem_fn]
pub unsafe fn region0_load_int8<M: Memory>(mem: &mut M, offset:u64) -> i32 {
    let mut byte : [u8; 1] = [0];
    region0_load(mem, offset, &mut byte);
    byte[0] as i32
}

#[ic_mem_fn]
pub unsafe fn region0_load_nat16<M: Memory>(mem: &mut M, offset:u64) -> u32 {
    let mut bytes : [u8; 2] = [0; 2];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u16::from_le_bytes(bytes).into()

}

#[ic_mem_fn]
pub unsafe fn region0_load_int16<M: Memory>(mem: &mut M, offset:u64) -> i32 {
    let mut bytes : [u8; 2] = [0; 2];
    region0_load(mem, offset, &mut bytes);
    core::primitive::i16::from_le_bytes(bytes).into()
}

#[ic_mem_fn]
pub unsafe fn region0_load_nat32<M: Memory>(mem: &mut M, offset:u64) -> u32 {
    let mut bytes : [u8; 4] = [0; 4];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u32::from_le_bytes(bytes).into()

}

#[ic_mem_fn]
pub unsafe fn region0_load_int32<M: Memory>(mem: &mut M, offset:u64) -> i32 {
    let mut bytes : [u8; 4] = [0; 4];
    region0_load(mem, offset, &mut bytes);
    core::primitive::i32::from_le_bytes(bytes).into()

}

#[ic_mem_fn]
pub unsafe fn region0_load_nat64<M: Memory>(mem: &mut M, offset:u64) -> u64 {
    let mut bytes : [u8; 8] = [0; 8];
    region0_load(mem, offset, &mut bytes);
    core::primitive::u64::from_le_bytes(bytes).into()

}

#[ic_mem_fn]
pub unsafe fn region0_load_int64<M: Memory>(mem: &mut M, offset:u64) -> i64 {
    let mut bytes : [u8; 8] = [0; 8];
    region0_load(mem, offset, &mut bytes);
    core::primitive::i64::from_le_bytes(bytes).into()

}

#[ic_mem_fn]
pub unsafe fn region0_load_float64<M: Memory>(mem: &mut M, offset:u64) -> f64 {
    let mut bytes : [u8; 8] = [0; 8];
    region0_load(mem, offset, &mut bytes);
    core::primitive::f64::from_le_bytes(bytes).into()
}

// -- Region0 store operations.

#[ic_mem_fn]
pub unsafe fn region0_store_byte<M: Memory>(mem: &mut M, offset:u64, byte: u32) {
    let mut byte : [u8; 1] = [byte as u8];
    region0_store(mem, offset, &mut byte);
}

#[ic_mem_fn]
pub unsafe fn region0_store_nat16<M: Memory>(mem: &mut M, offset:u64, val:u32) {
    region0_store(mem, offset, &core::primitive::u16::to_le_bytes(val as u16))
}

#[ic_mem_fn]
pub unsafe fn region0_store_int16<M: Memory>(mem: &mut M, offset:u64, val:i32) {
    region0_store(mem, offset, &core::primitive::i16::to_le_bytes(val as i16))
}

#[ic_mem_fn]
pub unsafe fn region0_store_nat32<M: Memory>(mem: &mut M, offset:u64, val:u32) {
    region0_store(mem, offset, &core::primitive::u32::to_le_bytes(val as u32))
}

#[ic_mem_fn]
pub unsafe fn region0_store_int32<M: Memory>(mem: &mut M, offset:u64, val:i32) {
    region0_store(mem, offset, &core::primitive::i32::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_nat64<M: Memory>(mem: &mut M, offset:u64, val:u64) {
    region0_store(mem, offset, &core::primitive::u64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_int64<M: Memory>(mem: &mut M, offset:u64, val:i64) {
    region0_store(mem, offset, &core::primitive::i64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_float64<M: Memory>(mem: &mut M, offset:u64, val:f64) {
    region0_store(mem, offset, &core::primitive::f64::to_le_bytes(val))
}

#[ic_mem_fn]
pub unsafe fn region0_store_blob<M: Memory>(mem: &mut M, offset:u64, val:u32) {
    let blob : *const Blob = Value::from_ptr(val as usize).as_blob();
    let len = blob.len();
    let bytes = blob.payload_const();
    let bytes : &[u8] = core::slice::from_raw_parts(bytes, len.0 as usize);
    region0_store(mem, offset, bytes)
}

#[ic_mem_fn]
pub unsafe fn region0_load_blob<M: Memory>(mem: &mut M, offset:u64, val:u32) {
    let blob : *mut Blob = Value::from_ptr(val as usize).as_blob_mut();
    let len = blob.len();
    let bytes = blob.payload_addr();
    let bytes : &mut [u8] = core::slice::from_raw_parts_mut(bytes, len.0 as usize);
    region0_load(mem, offset, bytes)
}
