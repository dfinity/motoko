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

    fn u8_from_nat8(value: Value) -> u8;
    fn u16_from_nat16(value: Value) -> u16;
    fn u32_from_nat32(value: Value) -> u32;
    fn u64_from_nat64(value: Value) -> u64;

    fn i8_from_int8(value: Value) -> i8;
    fn i16_from_int16(value: Value) -> i16;
    fn i32_from_int32(value: Value) -> i32;
    fn i64_from_int64(value: Value) -> i64;

    fn nat8_from_u8(value: u8) -> Value;
    fn nat16_from_u16(value: u16) -> Value;
    fn nat32_from_u32(value: u32) -> Value;
    fn nat64_from_u64(value: u64) -> Value;

    fn int8_from_i8(value: i8) -> Value;
    fn int16_from_i16(value: i16) -> Value;
    fn int32_from_i32(value: i32) -> Value;
    fn int64_from_i64(value: i64) -> Value;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MotokoError {
    UnexpectedTag(Tag),
    #[allow(unused)]
    Custom(u64),
}

type MotokoResult<T> = Result<T, MotokoError>;

/// Wrapper for representing a `Vec<u8>` value as a `Blob` in Motoko.
#[derive(Clone, Debug)]
pub struct BlobVec(pub Vec<u8>);

/// Trait for converting a Motoko value to a Rust value.
pub trait FromValue: Sized {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self>;
}

/// Trait for converting a Rust value to a Motoko value.
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

impl FromValue for u8 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(u8_from_nat8(value))
    }
}
impl IntoValue for u8 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(nat8_from_u8(self))
    }
}
impl FromValue for u16 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(u16_from_nat16(value))
    }
}
impl IntoValue for u16 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(nat16_from_u16(self))
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
impl FromValue for u64 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(u64_from_nat64(value))
    }
}
impl IntoValue for u64 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(nat64_from_u64(self))
    }
}

impl FromValue for i8 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(i8_from_int8(value))
    }
}
impl IntoValue for i8 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(int8_from_i8(self))
    }
}
impl FromValue for i16 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(i16_from_int16(value))
    }
}
impl IntoValue for i16 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(int16_from_i16(self))
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
impl FromValue for i64 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(i64_from_int64(value))
    }
}
impl IntoValue for i64 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(int64_from_i64(self))
    }
}

impl FromValue for bool {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        // Do we want a function in `compile.ml` here?
        let raw_value = value.get_raw();
        assert!(
            raw_value == 0 || raw_value == 1,
            "Unexpected boolean value: {:b}",
            raw_value
        );
        Ok(raw_value != 0)
    }
}
impl IntoValue for bool {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(Value::from_raw(self as u32))
    }
}

impl FromValue for BlobVec {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_BLOB => {
                let blob = value.as_blob();
                let len = blob.len().as_u32();
                Ok(BlobVec((0..len).into_iter().map(|i| blob.get(i)).collect()))
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}
impl IntoValue for BlobVec {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value> {
        let vec = self.0;
        let value = alloc_blob(mem, Bytes(vec.len() as u32));
        let blob = value.as_blob_mut();
        let mut dest = blob.payload_addr();
        for item in vec.into_iter() {
            *dest = item;
            dest = dest.add(1);
        }
        Ok(allocation_barrier(value))
        // ^^ Is this allocation barrier necessary?
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_ARRAY => {
                let array = value.as_array();
                let len = array.len();
                (0..len)
                    .into_iter()
                    .map(|i| T::from_value(array.get(i), mem))
                    .collect()
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}
impl<T: IntoValue> IntoValue for Vec<T> {
    unsafe fn into_value(self, mem: &mut impl Memory) -> MotokoResult<Value> {
        let value = alloc_array(mem, self.len() as u32);
        let array = value.as_array();
        let mut dest = array.payload_addr();
        for item in self.into_iter() {
            *dest = item.into_value(mem)?;
            dest = dest.add(1);
        }
        Ok(allocation_barrier(value))
    }
}

impl FromValue for () {
    unsafe fn from_value(_value: Value, _mem: &mut impl Memory) -> MotokoResult<Self> {
        Ok(())
    }
}
impl IntoValue for () {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> MotokoResult<Value> {
        Ok(unit())
    }
}

impl<A: FromValue, B: FromValue> FromValue for (A, B) {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> MotokoResult<Self> {
        match value.tag() {
            TAG_ARRAY => {
                let array = value.as_array();
                assert_eq!(array.len(), 2, "Unexpected tuple length");
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
        let dest = array.payload_addr();
        *dest = self.0.into_value(mem)?;
        *(dest.add(1)) = self.1.into_value(mem)?;
        Ok(allocation_barrier(value))
    }
}

// Temporary examples

#[motoko]
unsafe fn empty() {}

#[motoko]
unsafe fn identity(value: Value) -> Value {
    value
}

#[motoko]
unsafe fn blob_modify(mut blob: BlobVec) -> BlobVec {
    blob.0.push('!' as u8);
    blob
}

#[motoko]
unsafe fn array_concat(a: Vec<Value>, b: Vec<Value>) -> Vec<Value> {
    [a, b].concat()
}

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

#[motoko]
unsafe fn div_rem(a: u32, b: u32) -> (u32, u32) {
    (a / b, a % b)
}

#[motoko]
unsafe fn bool_swap(a: bool, b: bool) -> (bool, bool) {
    (b, a)
}

// #[motoko]
// unsafe fn check_numbers(
//     a: u8,
//     b: i8,
//     c: u16,
//     d: i16,
//     e: u32,
//     f: i32,
//     g: u64,
//     h: i64,
// ) -> (u8, i8, u16, i16, u32, i32, u64, i64) {
//     (a, b, c, d, e, f, g, h)
// }

// [external-codegen]
