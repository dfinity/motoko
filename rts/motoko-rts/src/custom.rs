// Custom RTS function utilities

use alloc::vec::Vec;
use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    types::{Bytes, Tag, Value, TAG_BLOB},
};

#[derive(Clone, Debug, Eq, PartialEq)]
enum MotokoError {
    UnexpectedTag(Tag),
    #[allow(unused)]
    Custom(u64),
}

type MotokoResult<T> = Result<T, MotokoError>;

trait FromValue: Sized {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self>;
}

trait IntoValue {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value>;
}

impl FromValue for Vec<u8> {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_BLOB => {
                let blob = value.as_blob();
                let len = blob.len().as_u32(); // `usize` here?
                Ok((0..len).into_iter().map(|i| blob.get(i)).collect())
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}

impl IntoValue for Vec<u8> {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value> {
        let value = alloc_blob(mem, Bytes(self.len() as u32));
        let blob = value.as_blob_mut();
        let mut dest = blob.payload_addr();
        for i in 0..self.len() {
            *dest = self[i];
            dest = dest.add(1);
        }
        Ok(allocation_barrier(value))
    }
}

unsafe fn blob_fn(
    mem: &mut impl Memory,
    blob: Value,
    function: impl FnOnce(Vec<u8>) -> Vec<u8>,
) -> MotokoResult<Value> {
    function(Vec::from_value(blob, mem)?).into_value(mem)
}

// Temporary example
#[no_mangle]
pub unsafe extern "C" fn echo(value: u32) -> u32 {
    value
}

// Temporary example
#[ic_mem_fn]
unsafe fn modify_blob<M: Memory>(mem: &mut M, value: Value) -> Value {
    blob_fn(mem, value, |mut vec| {
        vec.push('!' as u8);
        vec
    }).unwrap()
}

// [external-codegen]
