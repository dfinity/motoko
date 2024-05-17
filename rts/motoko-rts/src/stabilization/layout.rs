//! The long-term layout definition of the stable format.
//!
//! The stable object graph resides a linear stable memory space.
//!
//! Pointers are serialized as 64-bit skewed offsets in that space.
//! The stable encoding of scalars is currently identical to the
//! main memory encoding, requiring the precise scalar tag as metadata.
//! If the main memory scalar representation changes in future, the
//! values need to be explicitly translated to the expected stable format.
//!
//! Each object uses a `StableTag` as header and is followed by
//! the object payload as outlined in the corresponding Rust structs.
//! Some objects, such as `StableArray`, `StableObject`, `StableBlob`,
//! and `StableBigNum` have a dynamic payload body in addition to a static
//! header.
//!
//! Not all heap memory object types are stabilized because some
//! of them are not stable types. New object types can be added
//! with backwards compatibility but encoding changes to existing stable
//! data types must be handled with extra care to ensure backwards compatibility.
//!
//! Note: For a potential downgrade to 32-bit, the 64-bit stable pointer
//! offsets can be scaled down by a factor `8` during the destabilization
//! such that they fit into 32-bit values during Cheney's graph-copy.

use crate::{
    barriers::allocation_barrier,
    constants::WORD_SIZE,
    memory::Memory,
    rts_trap_with,
    types::{
        size_of, Tag, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN, TAG_BIGINT, TAG_BITS64, TAG_BLOB,
        TAG_CONCAT, TAG_MUTBOX, TAG_OBJECT, TAG_OBJ_IND, TAG_REGION, TAG_SOME, TAG_VARIANT,
        TRUE_VALUE,
    },
};

use self::{
    stable_array::StableArray, stable_bigint::StableBigInt, stable_bits64::StableBits64,
    stable_blob::StableBlob, stable_concat::StableConcat, stable_mutbox::StableMutBox,
    stable_obj_ind::StableObjInd, stable_object::StableObject, stable_region::StableRegion,
    stable_some::StableSome, stable_variant::StableVariant,
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
mod stable_bits64;
mod stable_blob;
mod stable_concat;
mod stable_mutbox;
mod stable_obj_ind;
mod stable_object;
mod stable_region;
mod stable_some;
mod stable_variant;

/// Different kinds of objects used in the stable format.
#[repr(u64)]
#[derive(Clone, Copy, PartialEq)]
pub enum StableObjectKind {
    Array = 1,
    MutBox = 2,
    Object = 3,
    Blob = 4,
    Bits64 = 5,
    Region = 6,
    Variant = 7,
    Concat = 8,
    BigInt = 9,
    ObjInd = 10,
    Some = 11,
}

#[repr(C)]
pub struct StableTag(u64);

impl StableObjectKind {
    pub fn encode(&self) -> StableTag {
        StableTag(*self as u64)
    }
}

impl StableTag {
    pub fn decode(&self) -> StableObjectKind {
        const STABLE_TAG_ARRAY: u64 = StableObjectKind::Array as u64;
        const STABLE_TAG_MUTBOX: u64 = StableObjectKind::MutBox as u64;
        const STABLE_TAG_OBJECT: u64 = StableObjectKind::Object as u64;
        const STABLE_TAG_BLOB: u64 = StableObjectKind::Blob as u64;
        const STABLE_TAG_BITS64: u64 = StableObjectKind::Bits64 as u64;
        const STABLE_TAG_REGION: u64 = StableObjectKind::Region as u64;
        const STABLE_TAG_VARIANT: u64 = StableObjectKind::Variant as u64;
        const STABLE_TAG_CONCAT: u64 = StableObjectKind::Concat as u64;
        const STABLE_TAG_BIGINT: u64 = StableObjectKind::BigInt as u64;
        const STABLE_TAG_OBJIND: u64 = StableObjectKind::ObjInd as u64;
        const STABLE_TAG_SOME: u64 = StableObjectKind::Some as u64;
        match self.0 {
            STABLE_TAG_ARRAY => StableObjectKind::Array,
            STABLE_TAG_MUTBOX => StableObjectKind::MutBox,
            STABLE_TAG_OBJECT => StableObjectKind::Object,
            STABLE_TAG_BLOB => StableObjectKind::Blob,
            STABLE_TAG_BITS64 => StableObjectKind::Bits64,
            STABLE_TAG_REGION => StableObjectKind::Region,
            STABLE_TAG_VARIANT => StableObjectKind::Variant,
            STABLE_TAG_CONCAT => StableObjectKind::Concat,
            STABLE_TAG_BIGINT => StableObjectKind::BigInt,
            STABLE_TAG_OBJIND => StableObjectKind::ObjInd,
            STABLE_TAG_SOME => StableObjectKind::Some,
            _ => unsafe { rts_trap_with("Invalid tag") },
        }
    }
}

impl StableObjectKind {
    fn deserialize(tag: Tag) -> StableObjectKind {
        match tag {
            // During the marking phase of the incremental GC, the mutator can see
            // array slice information in the object tag.
            TAG_ARRAY | TAG_ARRAY_SLICE_MIN.. => StableObjectKind::Array,
            TAG_MUTBOX => StableObjectKind::MutBox,
            TAG_OBJECT => StableObjectKind::Object,
            TAG_BLOB => StableObjectKind::Blob,
            TAG_BITS64 => StableObjectKind::Bits64,
            TAG_REGION => StableObjectKind::Region,
            TAG_VARIANT => StableObjectKind::Variant,
            TAG_CONCAT => StableObjectKind::Concat,
            TAG_BIGINT => StableObjectKind::BigInt,
            TAG_OBJ_IND => StableObjectKind::ObjInd,
            TAG_SOME => StableObjectKind::Some,
            _ => unreachable!("tag {tag}"),
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub struct StableValue(u64);

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
        StableValue(Self::skew(address))
    }

    pub fn to_stable_address(&self) -> u64 {
        Self::unskew(self.0)
    }

    pub fn payload_address(&self) -> u64 {
        self.to_stable_address() + size_of::<StableTag>().to_bytes().as_usize() as u64
    }

    pub fn serialize(value: Value) -> Self {
        StableValue(value.get_raw() as u64)
    }

    pub fn deserialize(&self) -> Value {
        Value::from_raw(self.0 as usize)
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
    unsafe fn serialize_static_part(
        stable_memory: &mut StableMemoryStream,
        main_object: *mut T,
    ) -> Self;
    unsafe fn serialize_dynamic_part(
        _stable_memory: &mut StableMemoryStream,
        _main_object: *mut T,
    ) {
    }

    unsafe fn serialize(stable_memory: &mut StableMemoryStream, main_object: Value) {
        let stable_tag = StableObjectKind::deserialize(main_object.tag()).encode();
        let main_object = main_object.as_obj() as *mut T;
        stable_memory.write(&stable_tag);
        unsafe {
            let static_part = Self::serialize_static_part(stable_memory, main_object);
            stable_memory.write(&static_part);
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
    if context.serialization.pending_array_scanning() {
        StableArray::resume_scanning(context, translate);
        return;
    }
    let tag = context.serialization.to_space().read::<StableTag>();
    match tag.decode() {
        StableObjectKind::Array => StableArray::scan_serialized(context, translate),
        StableObjectKind::MutBox => StableMutBox::scan_serialized(context, translate),
        StableObjectKind::Object => StableObject::scan_serialized(context, translate),
        StableObjectKind::Blob => StableBlob::scan_serialized(context, translate),
        StableObjectKind::Bits64 => StableBits64::scan_serialized(context, translate),
        StableObjectKind::Region => StableRegion::scan_serialized(context, translate),
        StableObjectKind::Variant => StableVariant::scan_serialized(context, translate),
        StableObjectKind::Concat => StableConcat::scan_serialized(context, translate),
        StableObjectKind::BigInt => StableBigInt::scan_serialized(context, translate),
        StableObjectKind::ObjInd => StableObjInd::scan_serialized(context, translate),
        StableObjectKind::Some => StableSome::scan_serialized(context, translate),
    }
}

pub unsafe fn serialize(stable_memory: &mut StableMemoryStream, main_object: Value) {
    match StableObjectKind::deserialize(main_object.tag()) {
        StableObjectKind::Array => StableArray::serialize(stable_memory, main_object),
        StableObjectKind::MutBox => StableMutBox::serialize(stable_memory, main_object),
        StableObjectKind::Object => StableObject::serialize(stable_memory, main_object),
        StableObjectKind::Blob => StableBlob::serialize(stable_memory, main_object),
        StableObjectKind::Bits64 => StableBits64::serialize(stable_memory, main_object),
        StableObjectKind::Region => StableRegion::serialize(stable_memory, main_object),
        StableObjectKind::Variant => StableVariant::serialize(stable_memory, main_object),
        StableObjectKind::Concat => StableConcat::serialize(stable_memory, main_object),
        StableObjectKind::BigInt => StableBigInt::serialize(stable_memory, main_object),
        StableObjectKind::ObjInd => StableObjInd::serialize(stable_memory, main_object),
        StableObjectKind::Some => StableSome::serialize(stable_memory, main_object),
    }
}

pub unsafe fn deserialize<M: Memory>(
    main_memory: &mut M,
    stable_memory: &mut StableMemoryAccess,
    stable_object: StableValue,
) -> Value {
    let tag = stable_memory.read::<StableTag>(stable_object.to_stable_address());
    match tag.decode() {
        StableObjectKind::Array => {
            StableArray::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::MutBox => {
            StableMutBox::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Object => {
            StableObject::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Blob => {
            StableBlob::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Bits64 => {
            StableBits64::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Region => {
            StableRegion::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Variant => {
            StableVariant::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Concat => {
            StableConcat::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::BigInt => {
            StableBigInt::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::ObjInd => {
            StableObjInd::deserialize(main_memory, stable_memory, stable_object)
        }
        StableObjectKind::Some => {
            StableSome::deserialize(main_memory, stable_memory, stable_object)
        }
    }
}
