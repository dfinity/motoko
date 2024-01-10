//! The long-term layout definition of the stable format.
//!
//! The stable object graph resides a linear stable memory space.
//!
//! Pointers are serialized as skewed offsets divided by 2 in that space.
//! Scalar and pointer values are serialized as 64-bit `StableValue`
//! for a long-term perspective, even if main memory operates on 32-bit.
//! A 32-bit program stores `StableValue` that fit into 32-bit, and
//! scalar values beyond this width are boxed.
//! This means that a 32-bit and 64-bit program can always upgrade from
//! a 32-bit version, while the downgrade from 64-bit to a 32-bit program
//! is not supported and detected.
//!
//! Each object uses a `StableTag` as header and is followed by
//! the object payload as outlined in the corresponding Rust structs.
//! Some objects, such as `StableArray`, `StableObject`, `StableBlob`,
//! and `StableBigNum` have a dynamic payload body in an addition to a static
//! header.
//!
//! Not all heap memory object types are stabilized because some
//! of them are not stable types. New object types can be added
//! with backwards compatibility but encoding changes to existing stable
//! data types must be handled with extra care to ensure backwards compatibility.

use crate::{
    barriers::allocation_barrier,
    constants::WORD_SIZE,
    memory::Memory,
    types::{
        size_of, Tag, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN, TAG_BIGINT, TAG_BITS32, TAG_BITS64,
        TAG_BLOB, TAG_CONCAT, TAG_MUTBOX, TAG_OBJECT, TAG_OBJ_IND, TAG_REGION, TAG_SOME,
        TAG_VARIANT, TRUE_VALUE,
    },
};

use self::{
    stable_array::StableArray, stable_bigint::StableBigInt, stable_bits32::StableBits32,
    stable_bits64::StableBits64, stable_blob::StableBlob, stable_concat::StableConcat,
    stable_mutbox::StableMutBox, stable_obj_ind::StableObjInd, stable_object::StableObject,
    stable_region::StableRegion, stable_some::StableSome, stable_variant::StableVariant,
};

use super::{
    deserialization::stable_memory_access::StableMemoryAccess,
    serialization::{
        stable_memory_stream::{ScanStream, StableMemoryStream, WriteStream},
        SerializationContext,
    },
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
mod stable_some;
mod stable_variant;

/// Object tag in stable format. Encoded as a `i32` value.
#[repr(C)]
#[derive(Clone, Copy)]
enum StableTag {
    _None = 0,
    Array = 1,
    MutBox = 2,
    Object = 3,
    Blob = 4,
    Bits32 = 5, // Note: Can be removed in 64-bit heap support.
    Bits64 = 6,
    Region = 7,
    Variant = 8,
    Concat = 9,
    BigInt = 10,
    ObjInd = 11,
    Some = 12,
}

const _: () = assert!(core::mem::size_of::<StableTag>() == core::mem::size_of::<i32>());

impl StableTag {
    fn deserialize(tag: Tag) -> StableTag {
        match tag {
            // During the marking phase of the incremental GC, the mutator can see
            // array slice information in the object tag.
            TAG_ARRAY | TAG_ARRAY_SLICE_MIN.. => StableTag::Array,
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
            TAG_SOME => StableTag::Some,
            _ => unimplemented!("tag {tag}"),
        }
    }
}

/// Special sentinel value that does not exist for static or dynamic objects.
/// Skewed -5. Since 1 is already reserved to encode the boolean `true`.
/// Note: The stable addresses start at 0 (skewed u32::MAX) as they are relatived to the to-space.
pub const STABLE_NULL_POINTER: StableValue = StableValue(0xffff_ffff_ffff_fffb);
pub const STABLE_NULL_POINTER_32: Value = Value::from_raw(STABLE_NULL_POINTER.0 as u32);

const _: () = assert!(STABLE_NULL_POINTER.0 != TRUE_VALUE as u64);
const _: () = assert!(STABLE_NULL_POINTER.0 & 0b1 != 0);
const _: () = assert!((STABLE_NULL_POINTER.0 + 1) % WORD_SIZE as u64 == 0);

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub struct StableValue(u64);

/// Due to the 64-bit pointer encoding in the stable format, the serialized space can require the double
/// size than the main memory. Therefore, the stable pointers (offsets in stable memory) can also exceed
/// the 32-bit address space. This would be a problem on deserialization, where the stable pointers are first
/// copied to the main memory object with 32-bit pointer slots and then later patched during the deserialization
/// scan. Therefore, the stable pointer address is first divided by two and then skewed. This requires
/// WORD_SIZE >= 4, such that the dividied addresses are still even numbers that can be skewed.
const POINTER_SCALE_FACTOR: u64 = 2;

const _: () = assert!(WORD_SIZE >= 4);

impl StableValue {
    fn is_ptr(&self) -> bool {
        self.0 & 0b1 == 1 && self.0 != TRUE_VALUE as u64
    }

    fn skew(address: u64) -> u64 {
        address.wrapping_sub(1)
    }

    fn unskew(pointer: u64) -> u64 {
        debug_assert!(Self::from_raw(pointer).is_ptr());
        pointer.wrapping_add(1)
    }

    pub const fn from_raw(value: u64) -> Self {
        StableValue(value)
    }

    pub fn from_stable_address(address: u64) -> Self {
        debug_assert_eq!(address % WORD_SIZE as u64, 0);
        debug_assert!(address / POINTER_SCALE_FACTOR <= u32::MAX as u64);
        StableValue(Self::skew(address / POINTER_SCALE_FACTOR))
    }

    pub fn to_stable_address(&self) -> u64 {
        Self::unskew(self.0) * POINTER_SCALE_FACTOR
    }

    pub fn payload_address(&self) -> u64 {
        self.to_stable_address() + size_of::<StableTag>().to_bytes().as_usize() as u64
    }

    pub fn serialize(value: Value) -> Self {
        if value == STABLE_NULL_POINTER_32 {
            STABLE_NULL_POINTER
        } else if value.get_raw() == u32::MAX {
            // Skewed stable address 0 in 32-bit value
            StableValue(u64::MAX)
        } else {
            StableValue(value.get_raw() as u64)
        }
    }

    pub fn deserialize(&self) -> Value {
        if *self == STABLE_NULL_POINTER {
            STABLE_NULL_POINTER_32
        } else if self.0 == u64::MAX as u64 {
            // Skewed stable address 0 in 64-bit value
            Value::from_raw(u32::MAX)
        } else {
            Value::from_raw(checked_to_u32(self.0))
        }
    }
}

/// Scan the static part of the object.
trait StaticScanner<T> {
    // Updates potential pointers in the static part of the object.
    // Returns true if values have been updated.
    fn update_pointers<C, F: Fn(&mut C, T) -> T>(
        &mut self,
        _context: &mut C,
        _translate: &F,
    ) -> bool {
        false
    }
}

pub trait StableToSpace {
    fn to_space(&mut self) -> &mut StableMemoryStream;
}

trait Serializer<T>
where
    Self: Sized + StaticScanner<StableValue>,
{
    unsafe fn serialize_static_part(main_object: *mut T) -> Self;
    unsafe fn serialize_dynamic_part(
        _stable_memory: &mut StableMemoryStream,
        _main_object: *mut T,
    ) {
    }

    unsafe fn serialize(stable_memory: &mut StableMemoryStream, main_object: Value) {
        let stable_tag = StableTag::deserialize(main_object.tag());
        let main_object = main_object.as_obj() as *mut T;
        stable_memory.write(&stable_tag);
        unsafe {
            stable_memory.write(&Self::serialize_static_part(main_object));
            Self::serialize_dynamic_part(stable_memory, main_object);
        }
    }

    fn scan_serialized<
        'a,
        M,
        F: Fn(&mut SerializationContext<'a, M>, StableValue) -> StableValue,
    >(
        context: &mut SerializationContext<'a, M>,
        translate: &F,
    ) {
        let mut static_part = context.serialization.to_space().read::<Self>();
        if static_part.update_pointers(context, translate) {
            context.serialization.to_space().update(&static_part);
        }
        static_part.scan_serialized_dynamic(context, translate);
    }

    fn scan_serialized_dynamic<
        'a,
        M,
        F: Fn(&mut SerializationContext<'a, M>, StableValue) -> StableValue,
    >(
        &self,
        _context: &mut SerializationContext<'a, M>,
        _translate: &F,
    ) {
    }

    unsafe fn allocate_deserialized<M: Memory>(&self, main_memory: &mut M) -> Value {
        main_memory.alloc_words(size_of::<T>())
    }

    unsafe fn deserialize_static_part(&self, target_object: *mut T);

    unsafe fn deserialize_dynamic_part<M: Memory>(
        &self,
        _main_memory: &mut M,
        _stable_memory: &StableMemoryAccess,
        _stable_object: StableValue,
        _target_object: *mut T,
    ) {
    }

    unsafe fn deserialize<M: Memory>(
        main_memory: &mut M,
        stable_memory: &StableMemoryAccess,
        stable_object: StableValue,
    ) -> Value {
        let stable_address = stable_object.payload_address();
        let stable_static_part = stable_memory.read::<Self>(stable_address);
        let target = stable_static_part.allocate_deserialized(main_memory);
        let target_object = target.get_ptr() as *mut T;
        stable_static_part.deserialize_static_part(target_object);
        stable_static_part.deserialize_dynamic_part(
            main_memory,
            stable_memory,
            stable_object,
            target_object,
        );
        allocation_barrier(target)
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

fn write_padding_u64(stable_memory: &mut StableMemoryStream, byte_length: usize) {
    let rounded_length = round_to_u64(byte_length as u64);
    let padding = rounded_length - byte_length as u64;
    for _ in 0..padding {
        stable_memory.write(&0u8);
    }
}

pub fn scan_serialized<
    'a,
    M,
    F: Fn(&mut SerializationContext<'a, M>, StableValue) -> StableValue,
>(
    context: &mut SerializationContext<'a, M>,
    translate: &F,
) {
    let tag = context.serialization.to_space().read::<StableTag>();
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
        StableTag::Some => StableSome::scan_serialized(context, translate),
        StableTag::_None => unimplemented!(),
    }
}

pub unsafe fn serialize(stable_memory: &mut StableMemoryStream, main_object: Value) {
    match StableTag::deserialize(main_object.tag()) {
        StableTag::Array => StableArray::serialize(stable_memory, main_object),
        StableTag::MutBox => StableMutBox::serialize(stable_memory, main_object),
        StableTag::Object => StableObject::serialize(stable_memory, main_object),
        StableTag::Blob => StableBlob::serialize(stable_memory, main_object),
        StableTag::Bits32 => StableBits32::serialize(stable_memory, main_object),
        StableTag::Bits64 => StableBits64::serialize(stable_memory, main_object),
        StableTag::Region => StableRegion::serialize(stable_memory, main_object),
        StableTag::Variant => StableVariant::serialize(stable_memory, main_object),
        StableTag::Concat => StableConcat::serialize(stable_memory, main_object),
        StableTag::BigInt => StableBigInt::serialize(stable_memory, main_object),
        StableTag::ObjInd => StableObjInd::serialize(stable_memory, main_object),
        StableTag::Some => StableSome::serialize(stable_memory, main_object),
        StableTag::_None => unimplemented!(),
    }
}

pub unsafe fn deserialize<M: Memory>(
    main_memory: &mut M,
    stable_memory: &mut StableMemoryAccess,
    stable_object: StableValue,
) -> Value {
    let tag = stable_memory.read::<StableTag>(stable_object.to_stable_address());
    match tag {
        StableTag::Array => StableArray::deserialize(main_memory, stable_memory, stable_object),
        StableTag::MutBox => StableMutBox::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Object => StableObject::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Blob => StableBlob::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Bits32 => StableBits32::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Bits64 => StableBits64::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Region => StableRegion::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Variant => StableVariant::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Concat => StableConcat::deserialize(main_memory, stable_memory, stable_object),
        StableTag::BigInt => StableBigInt::deserialize(main_memory, stable_memory, stable_object),
        StableTag::ObjInd => StableObjInd::deserialize(main_memory, stable_memory, stable_object),
        StableTag::Some => StableSome::deserialize(main_memory, stable_memory, stable_object),
        StableTag::_None => unimplemented!(),
    }
}
