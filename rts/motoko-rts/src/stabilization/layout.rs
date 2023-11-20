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
    block_size, Array, Bits32, Bits64, Blob, MutBox, Object, Region, Tag, Value, Words, TAG_ARRAY,
    TAG_BITS32, TAG_BITS64, TAG_BLOB, TAG_MUTBOX, TAG_OBJECT, TAG_REGION, TRUE_VALUE,
};

use self::{
    stable_array::StableArray, stable_bits32::StableBits32, stable_bits64::StableBits64,
    stable_blob::StableBlob, stable_mutbox::StableMutBox, stable_object::StableObject,
    stable_region::StableRegion,
};

use super::{
    reader_writer::{ScanStream, StableMemorySpace, WriteStream},
    StableMemoryAccess,
};

mod stable_array;
mod stable_bits32;
mod stable_bits64;
mod stable_blob;
mod stable_mutbox;
mod stable_object;
mod stable_region;

type StableTag = u32;

const STABLE_TAG_ARRAY: StableTag = 1;
const STABLE_TAG_MUTBOX: StableTag = 2;
const STABLE_TAG_OBJECT: StableTag = 3;
const STABLE_TAG_BLOB: StableTag = 4;
const STABLE_TAG_BITS32: StableTag = 5; // Note: Can be removed in 64-bit heap support.
const STABLE_TAG_BITS64: StableTag = 6;
const STABLE_TAG_REGION: StableTag = 7;

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
    tag: StableTag,
}

impl StableHeader {
    unsafe fn tag(self: *const StableHeader) -> StableTag {
        (*self).tag
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
    fn stable_tag() -> StableTag;

    unsafe fn serialize_static_part(main_object: *mut T) -> Self;
    unsafe fn serialize_dynamic_part(_memory: &mut StableMemorySpace, _main_object: *mut T) {}

    unsafe fn serialize(memory: &mut StableMemorySpace, main_object: *mut T) {
        memory.write(&Self::stable_tag());
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

    unsafe fn deserialize_static_part(stable_object: *mut Self, target_address: Value) -> T;
    unsafe fn deserialize_dynamic_part(_memory: &mut StableMemorySpace, _stable_object: *mut Self) {
    }

    unsafe fn deserialize(
        memory: &mut StableMemorySpace,
        stable_object: *mut Self,
        target_address: Value,
    ) {
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

pub fn scan_serialized<C: StableMemoryAccess, F: Fn(&mut C, StableValue) -> StableValue>(
    context: &mut C,
    translate: &F,
) {
    let tag = context.to_space().read::<StableTag>();
    match tag {
        STABLE_TAG_ARRAY => StableArray::scan_serialized(context, translate),
        STABLE_TAG_MUTBOX => StableMutBox::scan_serialized(context, translate),
        STABLE_TAG_OBJECT => StableObject::scan_serialized(context, translate),
        STABLE_TAG_BLOB => StableBlob::scan_serialized(context, translate),
        STABLE_TAG_BITS32 => StableBits32::scan_serialized(context, translate),
        STABLE_TAG_BITS64 => StableBits64::scan_serialized(context, translate),
        STABLE_TAG_REGION => StableRegion::scan_serialized(context, translate),
        other_tag => unimplemented!("other tag {other_tag}"),
    }
}

pub unsafe fn serialize(memory: &mut StableMemorySpace, object: Value) {
    match object.tag() {
        TAG_ARRAY => StableArray::serialize(memory, object.as_array()),
        TAG_MUTBOX => StableMutBox::serialize(memory, object.as_mutbox()),
        TAG_OBJECT => StableObject::serialize(memory, object.as_object()),
        TAG_BLOB => StableBlob::serialize(memory, object.as_blob_mut()),
        TAG_BITS32 => StableBits32::serialize(memory, object.as_bits32()),
        TAG_BITS64 => StableBits64::serialize(memory, object.as_bits64()),
        TAG_REGION => StableRegion::serialize(memory, object.as_region()),
        other_tag => unimplemented!("other tag {other_tag}"),
    }
}

pub fn scan_deserialized<C: StableMemoryAccess, F: Fn(&mut C, Value) -> Value>(
    context: &mut C,
    tag: Tag,
    translate: &F,
) {
    match tag {
        TAG_ARRAY => StableArray::scan_deserialized(context, translate),
        TAG_MUTBOX => StableMutBox::scan_deserialized(context, translate),
        TAG_OBJECT => StableObject::scan_deserialized(context, translate),
        TAG_BLOB => StableBlob::scan_deserialized(context, translate),
        TAG_BITS32 => StableBits32::scan_deserialized(context, translate),
        TAG_BITS64 => StableBits64::scan_deserialized(context, translate),
        TAG_REGION => StableRegion::scan_deserialized(context, translate),
        other_tag => unimplemented!("other tag {other_tag}"),
    }
}

pub unsafe fn deserialized_size(stable_object: *mut StableHeader) -> Words<u32> {
    match stable_object.tag() {
        STABLE_TAG_ARRAY => deserialized_size_for::<Array, StableArray>(stable_object),
        STABLE_TAG_MUTBOX => deserialized_size_for::<MutBox, StableMutBox>(stable_object),
        STABLE_TAG_OBJECT => deserialized_size_for::<Object, StableObject>(stable_object),
        STABLE_TAG_BLOB => deserialized_size_for::<Blob, StableBlob>(stable_object),
        STABLE_TAG_BITS32 => deserialized_size_for::<Bits32, StableBits32>(stable_object),
        STABLE_TAG_BITS64 => deserialized_size_for::<Bits64, StableBits64>(stable_object),
        STABLE_TAG_REGION => deserialized_size_for::<Region, StableRegion>(stable_object),
        other_tag => unimplemented!("other tag {other_tag}"),
    }
}

unsafe fn deserialized_size_for<T: Default + StaticScanner<Value>, E: Serializer<T>>(
    stable_object: *mut StableHeader,
) -> Words<u32> {
    // This is a workaround for reusing the existing `block_size()` function to determine object size in main memory:
    // The main memory object is decoded without its dynamic payload, by using a dummy value for the unused Brooks
    // forwarding pointer in this temporarily decoded object.
    let unused_pointer = Value::from_ptr(0);
    let static_part =
        &mut E::deserialize_static_part(stable_object.as_object::<E>(), unused_pointer);
    block_size(static_part as *mut T as usize)
}

pub unsafe fn deserialize(
    memory: &mut StableMemorySpace,
    stable_object: *mut StableHeader,
    target_address: Value,
) {
    match stable_object.tag() {
        STABLE_TAG_ARRAY => StableArray::deserialize(
            memory,
            stable_object.as_object::<StableArray>(),
            target_address,
        ),
        STABLE_TAG_MUTBOX => StableMutBox::deserialize(
            memory,
            stable_object.as_object::<StableMutBox>(),
            target_address,
        ),
        STABLE_TAG_OBJECT => StableObject::deserialize(
            memory,
            stable_object.as_object::<StableObject>(),
            target_address,
        ),
        STABLE_TAG_BLOB => StableBlob::deserialize(
            memory,
            stable_object.as_object::<StableBlob>(),
            target_address,
        ),
        STABLE_TAG_BITS32 => StableBits32::deserialize(
            memory,
            stable_object.as_object::<StableBits32>(),
            target_address,
        ),
        STABLE_TAG_BITS64 => StableBits64::deserialize(
            memory,
            stable_object.as_object::<StableBits64>(),
            target_address,
        ),
        STABLE_TAG_REGION => StableRegion::deserialize(
            memory,
            stable_object.as_object::<StableRegion>(),
            target_address,
        ),
        other_tag => unimplemented!("other tag {other_tag}"),
    }
}
