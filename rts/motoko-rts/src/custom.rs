// Custom RTS function utilities

use alloc::vec::Vec;
use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_blob, Memory},
    types::{Bytes, Tag, Value, TAG_BLOB},
};

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

pub trait FromValues: Sized {
    type Values;
    unsafe fn from_values(values: Self::Values, mem: &mut impl Memory) -> MotokoResult<Self>;
}

pub trait IntoValues {
    type Values;
    unsafe fn into_values(self, mem: &mut impl Memory) -> MotokoResult<Self::Values>;
}

impl<A: FromValue> FromValues for A {
    type Values = Value;
    unsafe fn from_values(value: Self::Values, mem: &mut impl Memory) -> MotokoResult<Self> {
        A::from_value(value, mem)
    }
}

impl<A: IntoValue> IntoValues for A {
    type Values = Value;
    unsafe fn into_values(self, mem: &mut impl Memory) -> MotokoResult<Self::Values> {
        self.into_value(mem)
    }
}

// TODO: macro for tuple implementations

impl<A: FromValue, B: FromValue> FromValues for (A, B) {
    type Values = (Value, Value);
    unsafe fn from_values((a, b): Self::Values, mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok((A::from_value(a, mem)?, B::from_value(b, mem)?))
    }
}

impl<A: IntoValue, B: IntoValue> IntoValues for (A, B) {
    type Values = (Value, Value);
    unsafe fn into_values(self, mem: &mut impl Memory) -> MotokoResult<Self::Values> {
        Ok((self.0.into_value(mem)?, self.1.into_value(mem)?))
    }
}

unsafe fn wrap<T: FromValues, R: IntoValues>(
    mem: &mut impl Memory,
    values: T::Values,
    function: impl FnOnce(T) -> R,
) -> MotokoResult<R::Values> {
    function(T::from_values(values, mem)?).into_values(mem)
}

// Temporary example
#[no_mangle]
pub unsafe extern "C" fn echo(value: Value) -> Value {
    value
}

// Temporary example
#[ic_mem_fn]
unsafe fn modify_blob<M: Memory>(mem: &mut M, value: Value) -> Value {
    wrap(mem, value, |mut vec: Vec<u8>| {
        vec.push('!' as u8);
        vec
    })
    .unwrap()
}

// [external-codegen]
