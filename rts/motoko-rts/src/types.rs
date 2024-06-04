// Note [struct representation]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// TLDR: Add `#[repr(C)]` in types used by both the runtime system and generated code, and add
// assertions to `static_assertions` module to check that the object size and field offsets are as
// expected.
//
// Rust compiler is free to reorder fields[1]. To avoid this in types that are used by both the
// runtime system and generated code we need a `repr` attribute like `repr(C)` or `repr(packed)`.
//
// We can't use `repr(packed)` because it potentially introduces undefined behavior as getting
// address (reference or pointer) of an unaligned field (or using the address) is an undefined
// behavior in Rust. See Motoko PR #2764.
//
// So to avoid reordering fields without introducing undefined behaviors we use `repr(C)`. See [2]
// for details on how `repr(C)` works. In short: it does not reorder fields. It can add padding,
// but in our case all fields are word-sized so that's not a problem.
//
// [1]: https://github.com/rust-lang/reference/blob/master/src/types/struct.md
// [2]: https://doc.rust-lang.org/stable/reference/type-layout.html#the-c-representation

use crate::barriers::{init_with_barrier, write_with_barrier};
use crate::memory::Memory;
use crate::tommath_bindings::{mp_digit, mp_int};
use core::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};
use core::ptr::null;
use motoko_rts_macros::is_incremental_gc;
use motoko_rts_macros::*;

use crate::constants::WORD_SIZE;
use crate::rts_trap_with;

pub fn size_of<T>() -> Words<u32> {
    Bytes(::core::mem::size_of::<T>() as u32).to_words()
}

/// The unit "words": `Words(123u32)` means 123 words.
#[repr(transparent)]
#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Words<A>(pub A);

impl Words<u32> {
    pub fn to_bytes(self) -> Bytes<u32> {
        Bytes(self.0 * WORD_SIZE)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl<A: Add<Output = A>> Add for Words<A> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Words(self.0 + rhs.0)
    }
}

impl<A: Sub<Output = A>> Sub for Words<A> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Words(self.0 - rhs.0)
    }
}

impl<A: Mul<Output = A>> Mul<A> for Words<A> {
    type Output = Self;

    fn mul(self, rhs: A) -> Self::Output {
        Words(self.0 * rhs)
    }
}

impl<A: Div<Output = A>> Div<A> for Words<A> {
    type Output = Self;

    fn div(self, rhs: A) -> Self::Output {
        Words(self.0 / rhs)
    }
}

impl<A: AddAssign> AddAssign for Words<A> {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl<A: SubAssign> SubAssign for Words<A> {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl From<Bytes<u32>> for Words<u32> {
    fn from(bytes: Bytes<u32>) -> Words<u32> {
        bytes.to_words()
    }
}

/// The unit "bytes": `Bytes(123u32)` means 123 bytes.
#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Bytes<A>(pub A);

impl Bytes<u32> {
    // Rounds up
    pub fn to_words(self) -> Words<u32> {
        // Rust issue for adding ceiling_div: https://github.com/rust-lang/rfcs/issues/2844
        Words((self.0 + WORD_SIZE - 1) / WORD_SIZE)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl<A: Add<Output = A>> Add for Bytes<A> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Bytes(self.0 + rhs.0)
    }
}

impl<A: Sub<Output = A>> Sub for Bytes<A> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Bytes(self.0 - rhs.0)
    }
}

impl<A: AddAssign> AddAssign for Bytes<A> {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl<A: SubAssign> SubAssign for Bytes<A> {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl From<Words<u32>> for Bytes<u32> {
    fn from(words: Words<u32>) -> Bytes<u32> {
        words.to_bytes()
    }
}

// The `true` value. The only scalar value that has the lowest bit set.
pub const TRUE_VALUE: u32 = 0x1;

/// A value in a heap slot
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Value(u32);

/// A view of `Value` for analyzing the slot contents.
pub enum PtrOrScalar {
    /// Slot is a pointer to a boxed object
    Ptr(usize),

    /// Slot is an unboxed scalar value
    Scalar(u32),
}

impl PtrOrScalar {
    pub fn is_ptr(&self) -> bool {
        matches!(self, PtrOrScalar::Ptr(_))
    }

    pub fn is_scalar(&self) -> bool {
        matches!(self, PtrOrScalar::Scalar(_))
    }
}

impl Value {
    /// Create a value from a pointer
    pub const fn from_ptr(ptr: usize) -> Self {
        // Cannot use `debug_assert_eq` in const yet, so using `debug_assert`
        debug_assert!(ptr & 0b1 == 0b0);
        Value(skew(ptr) as u32)
    }

    /// Create a value from a scalar
    pub const fn from_scalar(value: u32) -> Self {
        // Cannot use `debug_assert_eq` in const yet, so using `debug_assert`
        debug_assert!(value >> 31 == 0);
        Value(value << 1)
    }

    /// Create a value from a signed scalar. The scalar must be obtained with `get_signed_scalar`.
    /// Using `get_scalar` will return an incorrect scalar.
    pub fn from_signed_scalar(value: i32) -> Self {
        debug_assert_eq!(value, value << 1 >> 1);
        Value((value << 1) as u32)
    }

    /// Create a value from raw representation. Useful when e.g. temporarily writing invalid values
    /// to object fields in garbage collection.
    pub const fn from_raw(raw: u32) -> Self {
        Value(raw)
    }

    /// Analyzes the value.
    ///
    /// Note: when using this function in performance critical code make sure to check the
    /// generated Wasm and see if it can be improved by using `Value::get_raw`, `unskew`, etc.
    /// rustc/LLVM generates slightly more inefficient code (compared to using functions like
    /// `Value::get_raw` and `unskew`) in our cost model where every Wasm instruction costs 1
    /// cycle.
    pub fn get(&self) -> PtrOrScalar {
        if is_ptr(self.0) {
            PtrOrScalar::Ptr(unskew(self.0 as usize))
        } else {
            PtrOrScalar::Scalar(self.0 >> 1)
        }
    }

    /// Get the raw value
    #[inline]
    pub fn get_raw(&self) -> u32 {
        self.0
    }

    /// Is the value a scalar?
    pub fn is_scalar(&self) -> bool {
        self.get().is_scalar()
    }

    /// Is the value a pointer?
    pub fn is_ptr(&self) -> bool {
        self.get().is_ptr()
    }

    /// Assumes that the value is a scalar and returns the scalar value. In debug mode panics if
    /// the value is not a scalar.
    pub fn get_scalar(&self) -> u32 {
        debug_assert!(self.get().is_scalar());
        self.0 >> 1
    }

    /// Assumes that the value is a signed scalar and returns the scalar value. In debug mode
    /// panics if the value is not a scalar.
    pub fn get_signed_scalar(&self) -> i32 {
        debug_assert!(self.get().is_scalar());
        self.0 as i32 >> 1
    }

    /// Assumes that the value is a pointer and returns the pointer value. In debug mode panics if
    /// the value is not a pointer.
    pub fn get_ptr(self) -> usize {
        debug_assert!(self.get().is_ptr());
        unskew(self.0 as usize)
    }

    /// Check that the forwarding pointer is valid.
    #[inline]
    pub unsafe fn check_forwarding_pointer(self) {
        if is_incremental_gc!() {
            debug_assert!(
                self.forward().get_ptr() == self.get_ptr()
                    || self.forward().forward().get_ptr() == self.forward().get_ptr()
            );
        }
    }

    /// Check whether the object's forwarding pointer refers to a different location.
    pub unsafe fn is_forwarded(self) -> bool {
        if is_incremental_gc!() {
            self.check_forwarding_pointer();
            self.forward().get_ptr() != self.get_ptr()
        } else {
            false
        }
    }

    /// Get the object tag. No forwarding. Can be applied to any block, regular objects
    /// with a header as well as `OneWordFiller`, `FwdPtr`, and `FreeSpace`.
    /// In debug mode panics if the value is not a pointer.
    pub unsafe fn tag(self) -> Tag {
        *(self.get_ptr() as *const Tag)
    }

    /// Get the forwarding pointer. Used by the incremental GC.
    #[incremental_gc]
    pub unsafe fn forward(self) -> Value {
        debug_assert!(self.is_obj());
        debug_assert!(self.get_ptr() as *const Obj != null());
        let obj = self.get_ptr() as *const Obj;
        (*obj).forward
    }

    /// Get the forwarding pointer. Used by the incremental GC.
    #[non_incremental_gc]
    pub unsafe fn forward(self) -> Value {
        self
    }

    /// Resolve forwarding if the value is a pointer. Otherwise, return the same value.
    pub unsafe fn forward_if_possible(self) -> Value {
        if is_incremental_gc!() && self.is_ptr() && self.get_ptr() as *const Obj != null() {
            // Ignore null pointers used in text_iter.
            self.forward()
        } else {
            self
        }
    }

    /// Determines whether the value refers to an object with a regular header that contains a forwarding pointer.
    /// Returns `false` for pointers to special `OneWordFiller`, `FwdPtr`, and `FreeSpace` blocks that do not have
    /// a regular object header.
    pub unsafe fn is_obj(self) -> bool {
        let tag = self.tag();
        tag != TAG_FWD_PTR && tag != TAG_ONE_WORD_FILLER && tag != TAG_FREE_SPACE
    }

    pub unsafe fn is_blob(self) -> bool {
        let tag = self.tag();
        tag == TAG_BLOB_B || tag == TAG_BLOB_T || tag == TAG_BLOB_P || tag == TAG_BLOB_A
    }

    pub unsafe fn is_array(self) -> bool {
        let tag = self.tag();
        tag == TAG_ARRAY_I
            || tag == TAG_ARRAY_M
            || tag == TAG_ARRAY_T
            || tag == TAG_ARRAY_S
            || tag >= TAG_ARRAY_SLICE_MIN
    }

    /// Get the pointer as `Obj` using forwarding. In debug mode panics if the value is not a pointer.
    pub unsafe fn as_obj(self) -> *mut Obj {
        debug_assert!(self.get().is_ptr());
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Obj
    }

    /// Get the pointer as `MutBox` using forwarding. In debug mode panics if the value is not a pointer.
    pub unsafe fn as_mutbox(self) -> *mut MutBox {
        debug_assert_eq!(self.tag(), TAG_MUTBOX);
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut MutBox
    }

    /// Get the pointer as `Array` using forwarding. In debug mode panics if the value is not a pointer or the
    /// pointed object is not an `Array`.
    pub unsafe fn as_array(self) -> *mut Array {
        debug_assert!(self.is_array());
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Array
    }

    /// Get the pointer as `Region` using forwarding.
    pub unsafe fn as_region(self) -> *mut Region {
        debug_assert!(self.tag() == TAG_REGION);
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Region
    }

    /// Get the pointer as `Region` using forwarding, without checking the tag.
    /// NB: One cannot check the tag during stabilization.
    pub unsafe fn as_untagged_region(self) -> *mut Region {
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Region
    }

    /// Get the pointer as `Concat` using forwarding. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `Concat`.
    pub unsafe fn as_concat(self) -> *const Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *const Concat
    }

    /// Get the pointer as `Blob` using forwarding. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `Blob`.
    pub unsafe fn as_blob(self) -> *const Blob {
        debug_assert!(self.is_blob());
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *const Blob
    }

    /// Get the pointer as mutable `Blob` using forwarding.
    pub unsafe fn as_blob_mut(self) -> *mut Blob {
        debug_assert!(self.is_blob());
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Blob
    }

    /// Get the pointer as `Stream` using forwarding, which is a glorified `Blob`.
    /// In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `Blob`.
    pub unsafe fn as_stream(self) -> *mut Stream {
        debug_assert_eq!(self.tag(), TAG_BLOB_B);
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut Stream
    }

    /// Get the pointer as `BigInt` using forwarding. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `BigInt`.
    pub unsafe fn as_bigint(self) -> *mut BigInt {
        debug_assert_eq!(self.tag(), TAG_BIGINT);
        self.check_forwarding_pointer();
        self.forward().get_ptr() as *mut BigInt
    }

    pub fn as_tiny(self) -> i32 {
        debug_assert!(self.is_scalar());
        self.0 as i32 >> 1
    }

    // optimized version of `value.is_ptr() && value.get_ptr() >= address`
    // value is a pointer equal or greater than the unskewed address > 1
    #[inline]
    pub fn points_to_or_beyond(&self, address: usize) -> bool {
        debug_assert!(address > TRUE_VALUE as usize);
        let raw = self.get_raw();
        is_skewed(raw) && unskew(raw as usize) >= address
    }
}

#[inline]
/// Returns whether a raw value is representing a pointer. Useful when using `Value::get_raw`.
pub fn is_ptr(value: u32) -> bool {
    is_skewed(value) && value != TRUE_VALUE
}

#[inline]
pub const fn is_skewed(value: u32) -> bool {
    value & 0b1 != 0
}

#[inline]
pub const fn skew(ptr: usize) -> usize {
    ptr.wrapping_sub(1)
}

#[inline]
pub const fn unskew(value: usize) -> usize {
    value.wrapping_add(1)
}

// NOTE: We don't create an enum for tags as we can never assume to do exhaustive pattern match on
// tags, because of heap corruptions and other bugs (in the code generator or RTS, or maybe because
// of an unsafe API usage).
pub type Tag = u32;

// Tags need to have the lowest bit set, to allow distinguishing a header (tag) from object
// locations in mark-compact GC. (Reminder: objects and fields are word aligned)
pub const TAG_OBJECT: Tag = 1;
pub const TAG_OBJ_IND: Tag = 3;
pub const TAG_ARRAY_I: Tag = 5;   // Immutable Array ([T])
pub const TAG_ARRAY_M: Tag = 7;   // Mutable Array ([var T])
pub const TAG_ARRAY_T: Tag = 9;   // Non-nullary Tuple ((T,+))
pub const TAG_ARRAY_S: Tag = 11;  // Shared function pairing TAG_BLOB_A with TAG_BLOB_T (shared ... -> ...)
pub const TAG_BITS64_U: Tag = 13; // Unsigned (Nat64)
pub const TAG_BITS64_S: Tag = 15; // Signed (Int64)
pub const TAG_BITS64_F: Tag = 17; // Float
pub const TAG_MUTBOX: Tag = 19;
pub const TAG_CLOSURE: Tag = 21;
pub const TAG_SOME: Tag = 23;
pub const TAG_VARIANT: Tag = 25;
pub const TAG_BLOB_B: Tag = 27;   // Blob of Bytes (Blob)
pub const TAG_BLOB_T: Tag = 29;   // Blob of Utf8 (Text)
pub const TAG_BLOB_P: Tag = 31;   // Principal (Principal)
pub const TAG_BLOB_A: Tag = 33;   // Actor (actor {})
pub const TAG_FWD_PTR: Tag = 35; // Only used by the copying GC - not to be confused with forwarding pointer in the header used for incremental GC.
pub const TAG_BITS32_U: Tag = 37; // Unsigned (Nat32)
pub const TAG_BITS32_S: Tag = 39; // Signed (Int32)
pub const TAG_BITS32_F: Tag = 41; // Reserved (Float32)
pub const TAG_BIGINT: Tag = 43;
pub const TAG_CONCAT: Tag = 45;
pub const TAG_REGION: Tag = 47;
pub const TAG_NULL: Tag = 49;
pub const TAG_ONE_WORD_FILLER: Tag = 51;
pub const TAG_FREE_SPACE: Tag = 53;

// Special value to visit only a range of array fields.
// This and all values above it are reserved and mean
// a slice of an array object (i.e. start index) for
// purposes of `visit_pointer_fields`.
// Invariant: the value of this (pseudo-)tag must be
//            higher than all other tags defined above
pub const TAG_ARRAY_SLICE_MIN: Tag = 54;

pub fn slice_tag(array_tag: Tag, slice_start: u32) -> Tag {
    debug_assert!(
        array_tag == TAG_ARRAY_I
            || array_tag == TAG_ARRAY_M
            || array_tag == TAG_ARRAY_T
            || array_tag == TAG_ARRAY_S
    );
    debug_assert!(slice_start >= TAG_ARRAY_SLICE_MIN && slice_start < (1 << 30));
    (((array_tag - TAG_ARRAY_I) / 2) << 30) | slice_start
}

pub fn slice_start(tag: Tag) -> (Tag, u32) {
    debug_assert!(
        tag == TAG_ARRAY_I
            || tag == TAG_ARRAY_M
            || tag == TAG_ARRAY_T
            || tag == TAG_ARRAY_S
            || tag >= TAG_ARRAY_SLICE_MIN
    );
    if tag >= TAG_ARRAY_SLICE_MIN {
        (TAG_ARRAY_I + (tag >> 30) * 2, tag << 2 >> 2)
    } else {
        (tag, 0)
    }
}

pub fn base_array_tag(tag: Tag) -> Tag {
    debug_assert!(
        tag == TAG_ARRAY_I
            || tag == TAG_ARRAY_M
            || tag == TAG_ARRAY_T
            || tag == TAG_ARRAY_S
            || tag >= TAG_ARRAY_SLICE_MIN
    );
    if tag >= TAG_ARRAY_SLICE_MIN {
        TAG_ARRAY_I + (tag >> 30) * 2
    } else {
        tag
    }
}

// Common parts of any object. Other object pointers can be coerced into a pointer to this.
#[repr(C)] // See the note at the beginning of this module
pub struct Obj {
    pub tag: Tag,
    // Cannot use `#[incremental_gc]` as Rust only allows non-macro attributes for fields.
    #[cfg(feature = "incremental_gc")]
    /// Forwarding pointer to support object moving in the incremental GC.
    pub forward: Value,
}

impl Obj {
    #[incremental_gc]
    pub fn init_forward(&mut self, value: Value) {
        self.forward = value;
    }

    #[non_incremental_gc]
    pub fn init_forward(&mut self, _value: Value) {}

    /// Check whether the object's forwarding pointer refers to a different location.
    #[incremental_gc]
    pub unsafe fn is_forwarded(self: *const Self) -> bool {
        (*self).forward.get_ptr() != self as usize
    }

    #[non_incremental_gc]
    pub unsafe fn is_forwarded(self: *const Self) -> bool {
        false
    }

    pub unsafe fn tag(self: *const Self) -> Tag {
        (*self).tag
    }

    pub unsafe fn as_blob(self: *mut Self) -> *mut Blob {
        debug_assert!(
            self.tag() == TAG_BLOB_B || self.tag() == TAG_BLOB_T || self.tag() == TAG_BLOB_P
        );
        self as *mut Blob
    }

    pub unsafe fn as_concat(self: *mut Self) -> *const Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self as *const Concat
    }
}

#[rustfmt::skip]
#[repr(C)] // See the note at the beginning of this module
pub struct Array {
    pub header: Obj,
    pub len: u32, // number of elements

    // Array elements follow, each u32 sized. We can't have variable-sized structs in Rust so we
    // can't add a field here for the elements.
    // https://doc.rust-lang.org/nomicon/exotic-sizes.html
}

impl Array {
    pub unsafe fn payload_addr(self: *const Self) -> *mut Value {
        self.offset(1) as *mut Value // skip array header
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> Value {
        let slot_addr = self.element_address(idx);
        *(slot_addr as *const Value)
    }

    /// Initialize the element of a newly created array.
    /// Uses a generational post-update barrier on pointer writes.
    /// No incremental pre-update barrier as the previous value is undefined.
    /// Resolve pointer forwarding for the written value if necessary.
    pub unsafe fn initialize<M: Memory>(self: *mut Self, idx: u32, value: Value, mem: &mut M) {
        let slot_addr = self.element_address(idx) as *mut Value;
        init_with_barrier(mem, slot_addr, value);
    }

    /// Write a pointer value to an array element.
    /// Uses a incremental pre-update barrier and a generational post-update barrier.
    /// Resolves pointer forwarding for the written value.
    pub unsafe fn set_pointer<M: Memory>(self: *mut Self, idx: u32, value: Value, mem: &mut M) {
        debug_assert!(value.is_ptr());
        let slot_addr = self.element_address(idx) as *mut Value;
        write_with_barrier(mem, slot_addr, value);
    }

    /// Write a scalar value to an array element.
    /// No need for a write barrier.
    pub unsafe fn set_scalar(self: *mut Self, idx: u32, value: Value) {
        debug_assert!(value.is_scalar());
        let slot_addr = self.element_address(idx);
        *(slot_addr as *mut Value) = value;
    }

    #[inline]
    unsafe fn element_address(self: *const Self, idx: u32) -> usize {
        debug_assert!(self.len() > idx);
        self.payload_addr() as usize + (idx * WORD_SIZE) as usize
    }

    pub unsafe fn len(self: *const Self) -> u32 {
        (*self).len
    }

    pub unsafe fn base_tag(self: *const Self) -> Tag {
        base_array_tag((*self).header.tag)
    }

    pub unsafe fn get_slice_start(self: *const Self) -> (Tag, u32) {
        slice_start((*self).header.tag)
    }

    pub unsafe fn set_slice_start(self: *mut Self, array_tag: Tag, start: u32) -> () {
        (*self).header.tag = slice_tag(array_tag, start)
    }

    pub unsafe fn restore_tag(self: *mut Self, array_tag: Tag) -> () {
        (*self).header.tag = array_tag;
    }
}

#[rustfmt::skip]
#[repr(C)] // See the note at the beginning of this module
pub struct Region {
    pub header: Obj,
    // 64-bit id split into lower and upper halves for alignment reasons
    pub id_lower: u32,
    pub id_upper: u32,
    pub page_count: u32,
    pub vec_pages: Value, // Blob of u16's (each a page block ID).
}

impl Region {
    pub unsafe fn write_id64(self: *mut Self, value: u64) {
        write64(&mut (*self).id_lower, &mut (*self).id_upper, value);
    }

    pub unsafe fn read_id64(self: *mut Self) -> u64 {
        read64((*self).id_lower, (*self).id_upper)
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct Object {
    pub header: Obj,
    pub size: u32,     // Number of elements
    pub hash_ptr: u32, // Pointer to static information about object field labels. Not important for GC (does not contain pointers).
}

impl Object {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut Value {
        self.add(1) as *mut Value // skip object header
    }

    pub(crate) unsafe fn size(self: *mut Self) -> u32 {
        (*self).size
    }

    #[cfg(debug_assertions)]
    pub(crate) unsafe fn get(self: *mut Self, idx: u32) -> Value {
        *self.payload_addr().add(idx as usize)
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct ObjInd {
    pub header: Obj,
    pub field: Value,
}

#[repr(C)] // See the note at the beginning of this module
pub struct Closure {
    pub header: Obj,
    pub funid: u32,
    pub size: u32, // number of elements
                   // other stuff follows ...
}

impl Closure {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut Value {
        self.offset(1) as *mut Value // skip closure header
    }

    pub(crate) unsafe fn size(self: *mut Self) -> u32 {
        (*self).size
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct Blob {
    pub header: Obj,
    pub len: Bytes<u32>,
    // data follows ..
}

impl Blob {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut u8 {
        self.add(1) as *mut u8 // skip blob header
    }

    pub unsafe fn payload_const(self: *const Self) -> *const u8 {
        self.add(1) as *mut u8 // skip blob header
    }

    pub unsafe fn len(self: *const Self) -> Bytes<u32> {
        (*self).len
    }

    pub unsafe fn get(self: *const Self, idx: u32) -> u8 {
        *self.payload_const().add(idx as usize)
    }

    pub unsafe fn set(self: *mut Self, idx: u32, byte: u8) {
        *self.payload_addr().add(idx as usize) = byte;
    }

    pub unsafe fn payload_addr_u16(self: *mut Self) -> *mut u16 {
        self.add(1) as *mut u16 // skip blob header
    }

    pub unsafe fn payload_const_u16(self: *const Self) -> *const u16 {
        self.add(1) as *mut u16 // skip blob header
    }

    pub unsafe fn get_u16(self: *const Self, idx: u32) -> u16 {
        *self.payload_const_u16().add(idx as usize)
    }

    pub unsafe fn set_u16(self: *mut Self, idx: u32, value: u16) {
        *self.payload_addr_u16().add(idx as usize) = value;
    }

    /// Shrink blob to the given size. Slop after the new size is filled with filler objects.
    pub unsafe fn shrink(self: *mut Self, new_len: Bytes<u32>) {
        let current_len_words = self.len().to_words();
        let new_len_words = new_len.to_words();

        debug_assert!(new_len_words <= current_len_words);

        let slop = current_len_words - new_len_words;

        if slop == Words(1) {
            let filler = (self.payload_addr() as *mut u32).add(new_len_words.as_usize())
                as *mut OneWordFiller;
            (*filler).tag = TAG_ONE_WORD_FILLER;
        } else if slop != Words(0) {
            debug_assert!(slop >= size_of::<FreeSpace>());
            let filler =
                (self.payload_addr() as *mut u32).add(new_len_words.as_usize()) as *mut FreeSpace;
            (*filler).tag = TAG_FREE_SPACE;
            (*filler).words = slop - size_of::<FreeSpace>();
        }

        (*self).len = new_len;
    }
}

/// Note: Do not declare 64-bit fields, as otherwise, the objects are expected to be 64-bit aligned.
/// This is not the case in the current heap design.
/// Moreover, fields would also get 64-bit aligned causing implicit paddding.

#[repr(C)] // See the note at the beginning of this module
pub struct Stream {
    pub header: Blob,

    /// Components of the 64-bit `ptr` value. Little-endian encoding.
    /// Use `read_ptr64()` and `write_ptr64()` to access.
    pub ptr_lower: u32,
    pub ptr_upper: u32,

    /// Components of the 64-bit `start` value. Little-endian encoding.
    /// Use `read_start64()` and `write_start64()` to access.
    pub start_lower: u32,
    pub start_upper: u32,

    /// Components of the 64-bit `limit` value. Little-endian encoding.
    /// Use `read_limit64()` and `write_limit64()` to access.
    pub limit_lower: u32,
    pub limit_upper: u32,

    pub outputter: fn(*mut Self, *const u8, Bytes<u32>) -> (),
    pub filled: Bytes<u32>, // cache data follows ..
}

impl Stream {
    pub unsafe fn is_forwarded(self: *const Self) -> bool {
        (self as *const Obj).is_forwarded()
    }

    pub unsafe fn as_blob_mut(self: *mut Self) -> *mut Blob {
        debug_assert!(!self.is_forwarded());
        self as *mut Blob
    }

    pub unsafe fn write_ptr64(self: *mut Self, value: u64) {
        write64(&mut (*self).ptr_lower, &mut (*self).ptr_upper, value);
    }

    pub unsafe fn read_ptr64(self: *const Self) -> u64 {
        read64((*self).ptr_lower, (*self).ptr_upper)
    }

    pub unsafe fn write_start64(self: *mut Self, value: u64) {
        write64(&mut (*self).start_lower, &mut (*self).start_upper, value);
    }

    pub unsafe fn read_start64(self: *const Self) -> u64 {
        read64((*self).start_lower, (*self).start_upper)
    }

    pub unsafe fn write_limit64(self: *mut Self, value: u64) {
        write64(&mut (*self).limit_lower, &mut (*self).limit_upper, value);
    }

    pub unsafe fn read_limit64(self: *const Self) -> u64 {
        read64((*self).limit_lower, (*self).limit_upper)
    }
}

pub fn read64(lower: u32, upper: u32) -> u64 {
    ((upper as u64) << u32::BITS) | lower as u64
}

pub fn write64(lower: &mut u32, upper: &mut u32, value: u64) {
    *upper = (value >> u32::BITS) as u32;
    *lower = (value & u32::MAX as u64) as u32;
}

/// Only used by the copying GC - not to be confused with the forwarding pointer in the general object header
/// that is used by the incremental GC.
/// A forwarding pointer placed by the copying GC in place of an evacuated object.
#[repr(C)] // See the note at the beginning of this module
pub struct FwdPtr {
    pub tag: Tag,
    pub fwd: Value,
}

#[repr(C)] // See the note at the beginning of this module
pub struct BigInt {
    pub header: Obj,
    /// The data following now must describe is the `mp_int` struct.
    /// The data pointer (mp_int.dp) is irrelevant, and will be changed to point to
    /// the data within this object before it is used.
    /// (NB: If we have a non-moving GC, we can make this an invariant)
    pub mp_int: mp_int,
    // data follows ..
}

impl BigInt {
    pub unsafe fn len(self: *mut Self) -> Bytes<u32> {
        Bytes(((*self).mp_int.alloc as usize * core::mem::size_of::<mp_digit>()) as u32)
    }

    pub unsafe fn payload_addr(self: *mut Self) -> *mut mp_digit {
        self.add(1) as *mut mp_digit // skip closure header
    }

    #[incremental_gc]
    pub unsafe fn forward(self: *mut Self) -> *mut Self {
        (*self).header.forward.as_bigint()
    }

    #[non_incremental_gc]
    pub unsafe fn forward(self: *mut Self) -> *mut Self {
        self
    }

    pub unsafe fn from_payload(ptr: *mut mp_digit) -> *mut Self {
        let bigint = (ptr as *mut u32).sub(size_of::<BigInt>().as_usize()) as *mut BigInt;
        bigint.forward()
    }

    /// Returns pointer to the `mp_int` struct
    ///
    /// It fixes up the dp pointer. Instead of doing it here
    /// this could be done on allocation and every object move.
    ///
    /// Note that this returns a `const` pointer. This is very nice, as together with the const
    /// annotation on the libtommath API, this should prevent us from passing this pointer to a
    /// libtommath function that tries to change it. For example, we cannot confuse input and
    /// output parameters of mp_add() this way.
    pub unsafe fn mp_int_ptr(self: *mut BigInt) -> *const mp_int {
        (*self).mp_int.dp = self.payload_addr();
        &(*self).mp_int
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct MutBox {
    pub header: Obj,
    pub field: Value,
}

#[repr(C)] // See the note at the beginning of this module
pub struct Some {
    pub header: Obj,
    pub field: Value,
}

#[repr(C)] // See the note at the beginning of this module
pub struct Variant {
    pub header: Obj,
    pub tag: u32,
    pub field: Value,
}

#[repr(C)] // See the note at the beginning of this module
pub struct Concat {
    pub header: Obj,
    pub n_bytes: Bytes<u32>,
    pub text1: Value,
    pub text2: Value,
}

impl Concat {
    pub unsafe fn text1(self: *const Self) -> Value {
        (*self).text1
    }

    pub unsafe fn text2(self: *const Self) -> Value {
        (*self).text2
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct Null {
    pub header: Obj,
}

#[repr(C)] // See the note at the beginning of this module
pub struct Bits64 {
    pub header: Obj,
    // We have two 32-bit fields instead of one 64-bit to avoid aligning the fields on 64-bit
    // boundary.
    bits_lo: u32,
    bits_hi: u32,
}

impl Bits64 {
    pub fn bits(&self) -> u64 {
        (u64::from(self.bits_hi) << 32) | u64::from(self.bits_lo)
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct Bits32 {
    pub header: Obj,
    pub bits: u32,
}

/// Marks one word empty space in heap
#[repr(C)] // See the note at the beginning of this module
pub struct OneWordFiller {
    pub tag: Tag,
}

/// Marks arbitrary sized emtpy space in heap
#[repr(C)] // See the note at the beginning of this module
pub struct FreeSpace {
    pub tag: Tag,
    pub words: Words<u32>,
}

impl FreeSpace {
    /// Size of the free space (includes object header)
    pub unsafe fn size(self: *mut Self) -> Words<u32> {
        (*self).words + size_of::<FreeSpace>()
    }
}

/// Returns the heap block size in words.
/// Handles both objects with header and forwarding pointer
/// and special blocks such as `OneWordFiller`, `FwdPtr`, and `FreeSpace`
/// that do not have a forwarding pointer.
pub(crate) unsafe fn block_size(address: usize) -> Words<u32> {
    let tag = *(address as *mut Tag);
    match tag {
        TAG_OBJECT => {
            let object = address as *mut Object;
            let size = object.size();
            size_of::<Object>() + Words(size)
        }

        TAG_OBJ_IND => size_of::<ObjInd>(),

        // `block_size` is not used during the incremental mark phase and
        // therefore, does not support array slicing.
        TAG_ARRAY_I | TAG_ARRAY_M | TAG_ARRAY_T | TAG_ARRAY_S => {
            let array = address as *mut Array;
            let size = array.len();
            size_of::<Array>() + Words(size)
        }

        TAG_BITS64_U | TAG_BITS64_S | TAG_BITS64_F => size_of::<Bits64>(),

        TAG_MUTBOX => size_of::<MutBox>(),

        TAG_CLOSURE => {
            let closure = address as *mut Closure;
            let size = closure.size();
            size_of::<Closure>() + Words(size)
        }

        TAG_SOME => size_of::<Some>(),

        TAG_VARIANT => size_of::<Variant>(),

        TAG_BLOB_B | TAG_BLOB_T | TAG_BLOB_P | TAG_BLOB_A => {
            let blob = address as *mut Blob;
            size_of::<Blob>() + blob.len().to_words()
        }

        TAG_FWD_PTR => {
            rts_trap_with("object_size: forwarding pointer");
        }

        TAG_BITS32_U | TAG_BITS32_S | TAG_BITS32_F => size_of::<Bits32>(),

        TAG_BIGINT => {
            let bigint = address as *mut BigInt;
            size_of::<BigInt>() + bigint.len().to_words()
        }

        TAG_CONCAT => size_of::<Concat>(),

        TAG_NULL => size_of::<Null>(),

        TAG_ONE_WORD_FILLER => size_of::<OneWordFiller>(),

        TAG_FREE_SPACE => {
            let free_space = address as *mut FreeSpace;
            free_space.size()
        }

        TAG_REGION => size_of::<Region>(),

        _ => {
            rts_trap_with("object_size: invalid object tag");
        }
    }
}
