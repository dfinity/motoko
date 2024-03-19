// Custom RTS function utilities

use core::marker::PhantomData;

use alloc::vec::Vec;
use motoko_rts_macros::{motoko, tuple_macro};

use crate::{
    barriers::allocation_barrier,
    memory::{alloc_array, alloc_blob, Memory},
    types::{Bytes, Tag, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN, TAG_BLOB},
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

pub type Result<T, E = MotokoError> = core::result::Result<T, E>;

/// Trait for converting a Motoko value to a Rust value.
pub trait FromValue: Sized {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> Result<Self>;
}

/// Trait for converting a Rust value to a Motoko value.
pub trait IntoValue {
    unsafe fn into_value(self, mem: &mut impl Memory) -> Result<Value>;
}

impl FromValue for Value {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(value)
    }
}
impl IntoValue for Value {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(self)
    }
}

impl FromValue for u8 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(u8_from_nat8(value))
    }
}
impl IntoValue for u8 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(nat8_from_u8(self))
    }
}
impl FromValue for u16 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(u16_from_nat16(value))
    }
}
impl IntoValue for u16 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(nat16_from_u16(self))
    }
}
impl FromValue for u32 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(u32_from_nat32(value))
    }
}
impl IntoValue for u32 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(nat32_from_u32(self))
    }
}
impl FromValue for u64 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(u64_from_nat64(value))
    }
}
impl IntoValue for u64 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(nat64_from_u64(self))
    }
}

impl FromValue for i8 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(i8_from_int8(value))
    }
}
impl IntoValue for i8 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(int8_from_i8(self))
    }
}
impl FromValue for i16 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(i16_from_int16(value))
    }
}
impl IntoValue for i16 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(int16_from_i16(self))
    }
}
impl FromValue for i32 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(i32_from_int32(value))
    }
}
impl IntoValue for i32 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(int32_from_i32(self))
    }
}
impl FromValue for i64 {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(i64_from_int64(value))
    }
}
impl IntoValue for i64 {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(int64_from_i64(self))
    }
}

impl FromValue for bool {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
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
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(Value::from_raw(self as u32))
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> Result<Self> {
        match value.tag() {
            TAG_ARRAY | TAG_ARRAY_SLICE_MIN.. => {
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
    unsafe fn into_value(self, mem: &mut impl Memory) -> Result<Value> {
        Ok(Array::from_vec(self, mem)?.into())
    }
}

impl FromValue for () {
    unsafe fn from_value(_value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(())
    }
}
impl IntoValue for () {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(unit())
    }
}

// Implement `FromValue` and `IntoValue` for tuples
#[tuple_macro(2, 20)]
macro_rules! tuple_impl {
    ($len:expr; $($name:ident = $index:tt),+) => {
        impl<$($name: FromValue),+> FromValue for ($($name),+) {
            unsafe fn from_value(value: Value, mem: &mut impl Memory) -> Result<Self> {
                const LENGTH: u32 = $len;
                match value.tag() {
                    TAG_ARRAY | TAG_ARRAY_SLICE_MIN.. => {
                        let array = value.as_array();
                        assert_eq!(array.len(), LENGTH, "Unexpected tuple length");
                        Ok((
                            $($name::from_value(array.get($index), mem)?),+
                        ))
                    }
                    tag => Err(MotokoError::UnexpectedTag(tag)),
                }
            }
        }
        impl<$($name: IntoValue),+> IntoValue for ($($name),+) {
            unsafe fn into_value(self, mem: &mut impl Memory) -> Result<Value> {
                const LENGTH: u32 = $len;
                let value = alloc_array(mem, LENGTH);
                let array = value.as_array();
                $(array.initialize($index, $name::into_value(self.$index, mem)?, mem);)+
                Ok(allocation_barrier(value))
            }
        }
    };
}

// Data structures

/// Wrapper for representing a `Vec<u8>` value as a `Blob` in Motoko.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlobVec(Vec<u8>);

impl BlobVec {
    pub fn vec(self) -> Vec<u8> {
        self.0
    }
}

impl From<Vec<u8>> for BlobVec {
    fn from(value: Vec<u8>) -> Self {
        BlobVec(value)
    }
}
impl From<BlobVec> for Vec<u8> {
    fn from(value: BlobVec) -> Self {
        value.0
    }
}
impl FromValue for BlobVec {
    unsafe fn from_value(value: Value, mem: &mut impl Memory) -> Result<Self> {
        Ok(BlobVec(Blob::from_value(value.into(), mem)?.into_vec()?))
    }
}
impl IntoValue for BlobVec {
    unsafe fn into_value(self, mem: &mut impl Memory) -> Result<Value> {
        Blob::from_vec(self.vec(), mem)?.into_value(mem)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Blob(Value);

impl Blob {
    pub unsafe fn from_vec(vec: Vec<u8>, mem: &mut impl Memory) -> Result<Self> {
        let value = alloc_blob(mem, Bytes(vec.len() as u32));
        let blob = value.as_blob_mut();
        let mut dest = blob.payload_addr();
        for item in vec.into_iter() {
            *dest = item;
            dest = dest.add(1);
        }
        Ok(Blob(allocation_barrier(value)))
    }

    pub unsafe fn into_vec(self) -> Result<Vec<u8>> {
        match self.0.tag() {
            TAG_BLOB => {
                let blob = self.0.as_blob();
                let len = blob.len().as_u32();
                Ok((0..len).into_iter().map(|i| blob.get(i)).collect())
            }
            tag => Err(MotokoError::UnexpectedTag(tag)),
        }
    }
}

impl From<Value> for Blob {
    fn from(value: Value) -> Self {
        Blob(value)
    }
}
impl From<Blob> for Value {
    fn from(value: Blob) -> Self {
        value.0
    }
}
impl FromValue for Blob {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(Blob::from(value))
    }
}
impl IntoValue for Blob {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(self.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array<T>(Value, PhantomData<T>);

impl<T> Array<T> {
    pub unsafe fn from_vec(vec: Vec<T>, mem: &mut impl Memory) -> Result<Self>
    where
        T: IntoValue,
    {
        let value = alloc_array(mem, vec.len() as u32);
        let array = value.as_array();
        for (i, item) in vec.into_iter().enumerate() {
            array.initialize(i as u32, item.into_value(mem)?, mem);
        }
        Ok(Array::from(allocation_barrier(value)))
    }

    pub unsafe fn concat(slice: impl AsRef<[Array<T>]>, mem: &mut impl Memory) -> Array<T> {
        let slice = slice.as_ref();
        let length: u32 = slice.iter().map(|array| array.len()).sum();
        let value = alloc_array(mem, length as u32);
        let array = value.as_array();
        let mut dest = array.payload_addr();
        for array in slice {
            for i in 0..array.len() {
                *dest = array.get(i);
                dest = dest.add(1);
            }
        }
        Array::from(allocation_barrier(value))
    }

    pub unsafe fn len(&self) -> u32 {
        self.0.as_array().len()
    }

    pub unsafe fn get(&self, index: u32) -> Value {
        self.0.as_array().get(index)
    }
}

impl<T> From<Value> for Array<T> {
    fn from(value: Value) -> Self {
        Array(value, PhantomData::default())
    }
}
impl<T> From<Array<T>> for Value {
    fn from(value: Array<T>) -> Self {
        value.0
    }
}
impl<T> FromValue for Array<T> {
    unsafe fn from_value(value: Value, _mem: &mut impl Memory) -> Result<Self> {
        Ok(Array::from(value))
    }
}
impl<T> IntoValue for Array<T> {
    unsafe fn into_value(self, _mem: &mut impl Memory) -> Result<Value> {
        Ok(self.into())
    }
}

// Temporary examples

#[motoko]
fn empty() {}

#[motoko]
fn identity(value: Value) -> Value {
    value
}

#[motoko]
fn div_rem(a: u32, b: u32) -> (u32, u32) {
    (a / b, a % b)
}

#[motoko]
unsafe fn array_concat_fast(
    a: Array<Value>,
    b: Array<Value>,
    #[memory] mem: &mut impl Memory,
) -> Array<Value> {
    Array::concat([a, b], mem)
}

#[motoko]
fn array_concat_slow(a: Vec<Value>, b: Vec<Value>) -> Vec<Value> {
    [a, b].concat() // slower but immediately usable within the Rust ecosystem
}

#[motoko]
fn blob_modify(blob: BlobVec) -> BlobVec {
    let mut vec = blob.vec();
    vec.push('!' as u8);
    vec.into()
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
fn bool_swap(a: bool, b: bool) -> (bool, bool) {
    (b, a)
}

#[motoko]
fn check_numbers(
    a: u8,
    b: i8,
    c: u16,
    d: i16,
    e: u32,
    f: i32,
    g: u64,
    h: i64,
) -> (u8, i8, u16, i16, u32, i32, u64, i64) {
    (a, b, c, d, e, f, g, h)
}

// [external-codegen]
