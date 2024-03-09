// Custom RTS function utilities

use alloc::vec::Vec;
use motoko_rts_macros::{ic_mem_fn, motoko};

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

pub trait FromArgs: Sized {
    type Args;
    unsafe fn from_args(values: Self::Args, mem: &mut impl Memory) -> MotokoResult<Self>;
}

pub trait IntoArgs {
    type Args;
    unsafe fn into_args(self, mem: &mut impl Memory) -> MotokoResult<Self::Args>;
}

impl<A: FromValue> FromArgs for A {
    type Args = Value;
    unsafe fn from_args(value: Self::Args, mem: &mut impl Memory) -> MotokoResult<Self> {
        A::from_value(value, mem)
    }
}

impl<A: IntoValue> IntoArgs for A {
    type Args = Value;
    unsafe fn into_args(self, mem: &mut impl Memory) -> MotokoResult<Self::Args> {
        self.into_value(mem)
    }
}

// TODO: macro for tuple implementations

impl<A: FromValue, B: FromValue> FromArgs for (A, B) {
    type Args = (Value, Value);
    unsafe fn from_args((a, b): Self::Args, mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok((A::from_value(a, mem)?, B::from_value(b, mem)?))
    }
}

impl<A: IntoValue, B: IntoValue> IntoArgs for (A, B) {
    type Args = (Value, Value);
    unsafe fn into_args(self, mem: &mut impl Memory) -> MotokoResult<Self::Args> {
        Ok((self.0.into_value(mem)?, self.1.into_value(mem)?))
    }
}

unsafe fn wrap<T: FromArgs, R: IntoArgs>(
    mem: &mut impl Memory,
    values: T::Args,
    function: impl FnOnce(T) -> R,
) -> MotokoResult<R::Args> {
    function(T::from_args(values, mem)?).into_args(mem)
}

// Temporary example
#[no_mangle]
pub unsafe extern "C" fn echo(value: Value) -> Value {
    value
}

// Temporary example
#[ic_mem_fn]
unsafe fn blob_modify<M: Memory>(mem: &mut M, value: Value) -> Value {
    wrap(mem, value, |mut vec: Vec<u8>| {
        vec.push('!' as u8);
        vec
    })
    .unwrap()
}

// Temporary example
#[motoko]
unsafe fn blob_concat(mem: &mut impl Memory, a: Vec<u8>, b: Vec<u8>) -> Vec<u8> {
    [a, b].concat()
}

// [external-codegen]
