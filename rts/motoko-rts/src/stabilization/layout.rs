//! The long-term layout definition of the stable format.
//!
//! The stable object graph resides a linear stable memory space.
//!
//! Pointers are serialized as skewed offsets in that space.
//! Scalar and pointer values are serialized as 64-bit `StableValue`
//! for a long-term perspective, even if main memory operates on 32-bit.
//! A 32-bit program stores `StableValue` that fit into 32-bit, and
//! scalar values beyond this width are boxed.
//! This means that a 32-bit and 64-bit program can always upgrade from
//! a 32-bit version, while the downgrade from 64-bit to a 32-bit program
//! is not supported and detected.
//!
//! Each object uses a `StableTag` as header an is followed by
//! the object payload as outlined in the corresponding Rust structs.
//! Some objects, such as `StableArray`, `StableObject`, `StableBlob`,
//! and `StableBigNum`
//!
//! Not all heap memory object types are stabilized because some
//! of them are not stable types. New object types can be added
//! with backwards compatibility but encoding changes to existing stable
//! data types must be handled extra care to ensure backwards compatibility.

use crate::types::{
    block_size, size_of, Tag, Value, Words, TAG_ARRAY, TAG_BIGINT, TAG_BITS32, TAG_BITS64,
    TAG_BLOB, TAG_CONCAT, TAG_MUTBOX, TAG_OBJECT, TAG_OBJ_IND, TAG_REGION, TAG_VARIANT, TRUE_VALUE,
};

use self::{
    stable_array::StableArray, stable_bigint::StableBigInt, stable_bits32::StableBits32,
    stable_bits64::StableBits64, stable_blob::StableBlob, stable_concat::StableConcat,
    stable_mutbox::StableMutBox, stable_obj_ind::StableObjInd, stable_object::StableObject,
    stable_region::StableRegion, stable_variant::StableVariant,
};

use super::{
    reader_writer::{ScanStream, StableMemorySpace, WriteStream},
    StableMemoryAccess,
};

mod stable_array;
mod stable_bigint;
mod stable_bits32;
mod stable_bits64;
mod stable_blob;
mod stable_concat;
mod stable_mutbox;
mod stable_obj_ind;
mod stable_object;
mod stable_region;
mod stable_variant;

/// Object tag in stable format. Encoded as a `i32` value.
#[repr(C)]
#[derive(Clone, Copy, Default)]
enum StableTag {
    #[default]
    None = 0,
    Array = 1,
    MutBox = 2,
    Object = 3,
    Blob = 4,
    Bits32 = 5, // Note: Can be removed in 64-bit heap support.
    Bits64 = 6,
    Region = 7,
    Variant = 9,
    Concat = 10,
    BigInt = 11,
    ObjInd = 12,
}

const _: () = assert!(core::mem::size_of::<StableTag>() == core::mem::size_of::<i32>());

impl StableTag {
    fn deserialize(tag: Tag) -> StableTag {
        match tag {
            TAG_ARRAY => StableTag::Array,
            TAG_MUTBOX => StableTag::MutBox,
            TAG_OBJECT => StableTag::Object,
            TAG_BLOB => StableTag::Blob,
            TAG_BITS32 => StableTag::Bits32,
            TAG_BITS64 => StableTag::Bits64,
            TAG_REGION => StableTag::Region,
            TAG_VARIANT => StableTag::Variant,
            TAG_CONCAT => StableTag::Concat,
            TAG_BIGINT => StableTag::BigInt,
            TAG_OBJ_IND => StableTag::ObjInd,
            _ => unimplemented!("tag {tag}"),
        }
    }
}

/// Special sentinel value that does not exist for static or dynamic objects.
/// Skewed -3. Since 1 is already reserved to encode the boolean `true`.
/// Note: The stable addresses start at 0 (skewed u32::MAX) as they are relatived to the to-space.
pub const STABLE_NULL_POINTER: StableValue = StableValue(0xffff_ffff_ffff_fffd);
pub const STABLE_NULL_POINTER_32: Value = Value::from_raw(STABLE_NULL_POINTER.0 as u32);

const _: () = assert!(STABLE_NULL_POINTER.0 != TRUE_VALUE as u64);
const _: () = assert!(STABLE_NULL_POINTER.0 & 0b1 != 0);

#[repr(C)]
#[derive(Default, Clone, Copy, PartialEq)]
pub struct StableValue(u64);

impl StableValue {
    fn is_ptr(&self) -> bool {
        self.0 & 0b1 == 1
    }

    fn skew(address: u64) -> u64 {
        address.wrapping_sub(1)
    }

    fn unskew(pointer: u64) -> u64 {
        pointer.wrapping_add(1)
    }

    pub fn from_address(address: u64) -> Self {
        StableValue(Self::skew(address))
    }

    pub fn to_address(&self) -> u64 {
        Self::unskew(self.0)
    }

    pub fn serialize(value: Value) -> Self {
        if value == STABLE_NULL_POINTER_32 {
            STABLE_NULL_POINTER
        } else if value.is_ptr() {
            StableValue::from_address(value.get_ptr() as u64)
        } else {
            StableValue(value.get_raw() as u64)
        }
    }

    pub fn deserialize(&self) -> Value {
        if *self == STABLE_NULL_POINTER {
            STABLE_NULL_POINTER_32
        } else if self.is_ptr() {
            Value::from_ptr(checked_to_usize(self.to_address()))
        } else {
            Value::from_raw(checked_to_u32(self.0))
        }
    }
}

/// Common stable object header in the front of the specific stable object layout,
/// e.g. `StableArray`, `StableObject`, etc.
pub struct StableHeader {
    stable_tag: StableTag,
}

impl StableHeader {
    unsafe fn tag(self: *const StableHeader) -> StableTag {
        (*self).stable_tag
    }

    unsafe fn as_object<T>(self: *mut StableHeader) -> *mut T {
        self.offset(1) as *mut T
    }
}

/// Scan the static part of the object.
trait StaticScanner<T> {
    // Updates potential pointers in the static part of the object.
    // Returns true if values have been updated.
    fn update_pointers<C: StableMemoryAccess, F: Fn(&mut C, T) -> T>(
        &mut self,
        _context: &mut C,
        _translate: &F,
    ) -> bool {
        false
    }
}

trait Serializer<T: Default + StaticScanner<Value>>
where
    Self: Sized + StaticScanner<StableValue> + Default,
{
    unsafe fn serialize_static_part(main_object: *mut T) -> Self;
    unsafe fn serialize_dynamic_part(_memory: &mut StableMemorySpace, _main_object: *mut T) {}

    unsafe fn serialize(memory: &mut StableMemorySpace, main_object: Value) {
        let stable_tag = StableTag::deserialize(main_object.tag());
        let main_object = main_object.as_obj() as *mut T;
        memory.write(&stable_tag);
        unsafe {
            memory.write(&Self::serialize_static_part(main_object));
            Self::serialize_dynamic_part(memory, main_object);
        }
    }

    fn scan_serialized<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        context: &mut C,
        translate: &F,
    ) {
        let mut static_part = context.to_space().read::<Self>();
        if static_part.update_pointers(context, translate) {
            context.to_space().update(&static_part);
        }
        Self::scan_serialized_dynamic(context, &static_part, translate);
    }

    fn scan_serialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
        _context: &mut C,
        _stable_object: &Self,
        _translate: &F,
    ) {
    }

    unsafe fn deserialized_size(stable_object: *mut StableHeader) -> Words<u32> {
        // This is a workaround for reusing the existing `block_size()` function to determine object size in main memory:
        // The main memory object is decoded without its dynamic payload, by using a dummy value for the unused Brooks
        // forwarding pointer in this temporarily decoded object.
        let unused_pointer = Value::from_ptr(0);
        let static_part =
            &mut Self::deserialize_static_part(stable_object.as_object::<Self>(), unused_pointer);
        block_size(static_part as *mut T as usize)
    }

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> T;
    unsafe fn deserialize_dynamic_part(_memory: &mut StableMemorySpace, _stable_object: *mut Self) {
    }

    unsafe fn deserialize(
        memory: &mut StableMemorySpace,
        stable_object: *mut StableHeader,
        target_address: Value,
    ) {
        let stable_object = stable_object.as_object::<Self>();
        memory.write(&Self::deserialize_static_part(
            stable_object,
            target_address,
        ));
        Self::deserialize_dynamic_part(memory, stable_object);
    }

    fn scan_deserialized<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
        translate: &F,
    ) {
        let mut static_part = context.to_space().read::<T>();
        if static_part.update_pointers(context, translate) {
            context.to_space().update(&static_part);
        }
        Self::scan_deserialized_dynamic(context, &static_part, translate);
    }

    fn scan_deserialized_dynamic<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
        _context: &mut C,
        _main_object: &T,
        _translate: &F,
    ) {
    }
}

pub fn checked_to_u32(input: u64) -> u32 {
    assert!(input <= u32::MAX as u64);
    input as u32
}

pub fn checked_to_usize(input: u64) -> usize {
    assert!(input <= usize::MAX as u64);
    input as usize
}

pub fn round_to_u64(length: u64) -> u64 {
    let alignment = size_of::<u64>().to_bytes().as_usize() as u64;
    (length + alignment - 1) / alignment * alignment
}

fn write_padding_u64(memory: &mut StableMemorySpace, byte_length: usize) {
    let rounded_length = round_to_u64(byte_length as u64);
    let padding = rounded_length - byte_length as u64;
    for _ in 0..padding {
        memory.write(&0u8);
    }
}

pub fn scan_serialized<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
    context: &mut C,
    translate: &F,
) {
    let tag = context.to_space().read::<StableTag>();
    match tag {
        StableTag::Array => StableArray::scan_serialized(context, translate),
        StableTag::MutBox => StableMutBox::scan_serialized(context, translate),
        StableTag::Object => StableObject::scan_serialized(context, translate),
        StableTag::Blob => StableBlob::scan_serialized(context, translate),
        StableTag::Bits32 => StableBits32::scan_serialized(context, translate),
        StableTag::Bits64 => StableBits64::scan_serialized(context, translate),
        StableTag::Region => StableRegion::scan_serialized(context, translate),
        StableTag::Variant => StableVariant::scan_serialized(context, translate),
        StableTag::Concat => StableConcat::scan_serialized(context, translate),
        StableTag::BigInt => StableBigInt::scan_serialized(context, translate),
        StableTag::ObjInd => StableObjInd::scan_serialized(context, translate),
        StableTag::None => unimplemented!(),
    }
}

pub unsafe fn serialize(memory: &mut StableMemorySpace, main_object: Value) {
    match StableTag::deserialize(main_object.tag()) {
        StableTag::Array => StableArray::serialize(memory, main_object),
        StableTag::MutBox => StableMutBox::serialize(memory, main_object),
        StableTag::Object => StableObject::serialize(memory, main_object),
        StableTag::Blob => StableBlob::serialize(memory, main_object),
        StableTag::Bits32 => StableBits32::serialize(memory, main_object),
        StableTag::Bits64 => StableBits64::serialize(memory, main_object),
        StableTag::Region => StableRegion::serialize(memory, main_object),
        StableTag::Variant => StableVariant::serialize(memory, main_object),
        StableTag::Concat => StableConcat::serialize(memory, main_object),
        StableTag::BigInt => StableBigInt::serialize(memory, main_object),
        StableTag::ObjInd => StableObjInd::serialize(memory, main_object),
        StableTag::None => unimplemented!(),
    }
}

pub fn scan_deserialized<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
    context: &mut C,
    tag: Tag,
    translate: &F,
) {
    match StableTag::deserialize(tag) {
        StableTag::Array => StableArray::scan_deserialized(context, translate),
        StableTag::MutBox => StableMutBox::scan_deserialized(context, translate),
        StableTag::Object => StableObject::scan_deserialized(context, translate),
        StableTag::Blob => StableBlob::scan_deserialized(context, translate),
        StableTag::Bits32 => StableBits32::scan_deserialized(context, translate),
        StableTag::Bits64 => StableBits64::scan_deserialized(context, translate),
        StableTag::Region => StableRegion::scan_deserialized(context, translate),
        StableTag::Variant => StableVariant::scan_deserialized(context, translate),
        StableTag::Concat => StableConcat::scan_deserialized(context, translate),
        StableTag::BigInt => StableBigInt::scan_deserialized(context, translate),
        StableTag::ObjInd => StableObjInd::scan_deserialized(context, translate),
        StableTag::None => unimplemented!(),
    }
}

pub unsafe fn deserialized_size(stable_object: *mut StableHeader) -> Words<u32> {
    match stable_object.tag() {
        StableTag::Array => StableArray::deserialized_size(stable_object),
        StableTag::MutBox => StableMutBox::deserialized_size(stable_object),
        StableTag::Object => StableObject::deserialized_size(stable_object),
        StableTag::Blob => StableBlob::deserialized_size(stable_object),
        StableTag::Bits32 => StableBits32::deserialized_size(stable_object),
        StableTag::Bits64 => StableBits64::deserialized_size(stable_object),
        StableTag::Region => StableRegion::deserialized_size(stable_object),
        StableTag::Variant => StableVariant::deserialized_size(stable_object),
        StableTag::Concat => StableConcat::deserialized_size(stable_object),
        StableTag::BigInt => StableBigInt::deserialized_size(stable_object),
        StableTag::ObjInd => StableObjInd::deserialized_size(stable_object),
        StableTag::None => unimplemented!(),
    }
}

pub unsafe fn deserialize(
    memory: &mut StableMemorySpace,
    stable_object: *mut StableHeader,
    target_address: Value,
) {
    match stable_object.tag() {
        StableTag::Array => StableArray::deserialize(memory, stable_object, target_address),
        StableTag::MutBox => StableMutBox::deserialize(memory, stable_object, target_address),
        StableTag::Object => StableObject::deserialize(memory, stable_object, target_address),
        StableTag::Blob => StableBlob::deserialize(memory, stable_object, target_address),
        StableTag::Bits32 => StableBits32::deserialize(memory, stable_object, target_address),
        StableTag::Bits64 => StableBits64::deserialize(memory, stable_object, target_address),
        StableTag::Region => StableRegion::deserialize(memory, stable_object, target_address),
        StableTag::Variant => StableVariant::deserialize(memory, stable_object, target_address),
        StableTag::Concat => StableConcat::deserialize(memory, stable_object, target_address),
        StableTag::BigInt => StableBigInt::deserialize(memory, stable_object, target_address),
        StableTag::ObjInd => StableObjInd::deserialize(memory, stable_object, target_address),
        StableTag::None => unimplemented!(),
    }
}
