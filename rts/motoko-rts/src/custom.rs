// Custom RTS function utilities

use alloc::vec::Vec;
use motoko_rts_macros::motoko;

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_array, alloc_blob, Memory},
    types::{Bytes, Tag, Value, TAG_ARRAY, TAG_BLOB},
};

extern "C" {
    fn unit() -> Value;
    fn u32_from_nat32(value: Value) -> u32;
    fn i32_from_int32(value: Value) -> i32;
    fn nat32_from_u32(value: u32) -> Value;
    fn int32_from_i32(value: i32) -> Value;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MotokoError {
    UnexpectedTag(Tag),
    #[allow(unused)]
    Custom(u64),
}

type MotokoResult<T> = Result<T, MotokoError>;

pub trait FromValue: Sized {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self>;
}

pub trait IntoValue {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value>;
}

impl FromValue for Value {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(value)
    }
}
impl IntoValue for Value {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(self)
    }
}

impl FromValue for () {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(())
    }
}
impl IntoValue for () {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(unit())
    }
}

impl FromValue for u32 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(u32_from_nat32(value))
    }
}
impl IntoValue for u32 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(nat32_from_u32(self))
    }
}

impl FromValue for i32 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(i32_from_int32(value))
    }
}
impl IntoValue for i32 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(int32_from_i32(self))
    }
}

impl FromValue for Vec<u8> {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_BLOB => {
                let blob = value.as_blob();
                let len = blob.len().as_u32();
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
        // ^^ Is this allocation barrier necessary?
    }
}

impl FromValue for Vec<Value> {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_ARRAY => {
                let array = value.as_array();
                let len = array.len();
                Ok((0..len).into_iter().map(|i| array.get(i)).collect())
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}
impl IntoValue for Vec<Value> {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value> {
        let value = alloc_array(mem, self.len() as u32);
        let array = value.as_array();
        let mut dest = array.payload_addr();
        for i in 0..self.len() {
            *dest = self[i];
            dest = dest.add(1);
        }
        Ok(allocation_barrier(value))
    }
}

impl<A: FromValue, B: FromValue> FromValue for (A, B) {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_ARRAY => {
                let array = value.as_array();
                let len = array.len();
                Ok((
                    A::from_value(array.get(0), mem)?,
                    B::from_value(array.get(1), mem)?,
                ))
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}
impl<A: IntoValue, B: IntoValue> IntoValue for (A, B) {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value> {
        let value = alloc_array(mem, 2);
        let array = value.as_array();
        let mut dest = array.payload_addr();
        *dest = self.0.into_value(mem)?;
        *(dest.add(1)) = self.1.into_value(mem)?;
        Ok(allocation_barrier(value))
    }
}

// Temporary example
#[motoko]
unsafe fn empty() {}

// Temporary example
#[motoko]
unsafe fn identity(value: Value) -> Value {
    value
}

// Temporary example
#[motoko]
unsafe fn blob_modify(mut vec: Vec<u8>) -> Vec<u8> {
    vec.push('!' as u8);
    vec
}

// Temporary example
#[motoko]
unsafe fn blob_concat(a: Vec<u8>, b: Vec<u8>) -> Vec<u8> {
    [a, b].concat()
}

#[motoko]
unsafe fn div_rem(a: u32, b: u32) -> (u32, u32) {
    (a / b, a % b)
}

// Temporary example
#[motoko]
unsafe fn manual_alloc(#[memory] mem: &mut impl Memory) -> Value {
    // Low-level access to memory allocation
    let value = alloc_blob(mem, Bytes(3 as u32));
    let blob = value.as_blob_mut();
    let mut dest = blob.payload_addr();
    for i in 0..3 {
        *dest = (i + 1) * 0x11;
        dest = dest.add(1);
    }
    allocation_barrier(value)
}

// [external-codegen]
