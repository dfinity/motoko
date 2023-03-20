//! Heap layout.
//!
//! Each object has its object id.
//! Objects in the heap are indirectly referred to via a central object table, that maps an
//! object id to the corresponding object address. Fields and array elements store values that
//! encode an object id if it refers to an object. Objects are handled uniformly, regardless
//! of whether allocated in the static or the dynamic heap.
//!
//! Value representation:
//! * Object id:
//!     Skewed offset of the offset in the object table that identifies the object.
//!     (Table entry offsets are aligned to 4 bytes, so the skewed representation has bit 0 set.)
//! * Scalar: A scalar value, shifted by 1 bit.
//!
//! Each object contains the object id in an extra header word, to allow fast reverse lookup of
//! the object id by a given object address.
//!
//! Object ids have bit 0 set (skewed), while scalar values have bit 0 clear (left-shifted by 1).
//!
//! Exceptions for non-incremental GC mode:
//!  * Object id = skewed object adddress. No indirection via the object table is used.

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

use crate::gc::generational::write_barrier::{
    generational_write_barrier, using_generational_barrier,
};
use crate::gc::incremental::object_table::{OBJECT_TABLE, OBJECT_TABLE_ID};
use crate::gc::incremental::write_barrier::{using_incremental_barrier, write_with_barrier};
use crate::memory::Memory;
use crate::tommath_bindings::{mp_digit, mp_int};
use core::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};
use core::ptr::null_mut;

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
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(u32);

/// Sentinel `null` reference, reserved object id trapping when resolving its address.
pub const NULL_OBJECT_ID: Value = Value::from_raw(skew(0) as u32);

impl Value {
    /// Obtain a new object id in the object table for a new object
    /// that resides at the defined address.
    pub unsafe fn new_object_id(address: usize) -> Value {
        debug_assert!(!is_skewed(address as u32));
        if OBJECT_TABLE != null_mut() {
            OBJECT_TABLE.new_object_id(address)
        } else {
            Value(skew(address) as u32)
        }
    }

    /// Free the object if for a deleted object.
    /// Called by the incremental GC for garbage objects.
    pub unsafe fn free_object_id(self) {
        if OBJECT_TABLE != null_mut() {
            OBJECT_TABLE.free_object_id(self);
        }
    }

    /// Record a new address for an object after it has been moved.
    /// Used by the incremental GC.
    pub unsafe fn set_new_address(self, new_address: usize) {
        debug_assert_ne!(OBJECT_TABLE, null_mut());
        debug_assert!(self != NULL_OBJECT_ID);
        debug_assert!(self != OBJECT_TABLE_ID);
        OBJECT_TABLE.move_object(self, new_address);
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

    /// Get the raw value
    #[inline]
    pub fn get_raw(&self) -> u32 {
        self.0
    }

    /// Is the value a scalar?
    pub fn is_scalar(&self) -> bool {
        !is_object_id(self.0)
    }

    /// Is the value a pointer?
    pub fn is_object_id(&self) -> bool {
        is_object_id(self.0)
    }

    /// Assumes that the value is a scalar and returns the scalar value. In debug mode panics if
    /// the value is not a scalar.
    pub fn get_scalar(&self) -> u32 {
        debug_assert!(self.is_scalar());
        self.0 >> 1
    }

    /// Assumes that the value is a signed scalar and returns the scalar value. In debug mode
    /// panics if the value is not a scalar.
    pub fn get_signed_scalar(&self) -> i32 {
        debug_assert!(self.is_scalar());
        self.0 as i32 >> 1
    }

    /// Get the address of an object by lookup through the object table.
    pub unsafe fn get_object_address(self) -> usize {
        assert!(self.is_object_id());
        if OBJECT_TABLE != null_mut() {
            OBJECT_TABLE.get_object_address(self)
        } else {
            // Non-incremental GC mode.
            unskew(self.0 as usize)
        }
    }

    /// Get the object tag. In debug mode panics if the value is not an object id.
    pub unsafe fn tag(self) -> Tag {
        *(self.get_object_address() as *const Tag)
    }

    /// Get the pointer as `Obj`. In debug mode panics if the value is not an object id.
    pub unsafe fn as_obj(self) -> *mut Obj {
        debug_assert!(has_object_header(self.tag()));
        self.get_object_address() as *mut Obj
    }

    /// Get the pointer as `MutBox`. In debug mode panics if the value is not an object id pointing to a `MutBox`.
    pub unsafe fn as_mutbox(self) -> *mut MutBox {
        debug_assert_eq!(self.tag(), TAG_MUTBOX);
        self.get_object_address() as *mut MutBox
    }

    /// Get the pointer as `Array`. In debug mode panics if the value is not an object id pointing to an `Array`.
    pub unsafe fn as_array(self) -> *mut Array {
        debug_assert!(self.tag() == TAG_ARRAY || self.tag() >= TAG_ARRAY_SLICE_MIN);
        self.get_object_address() as *mut Array
    }

    /// Get the pointer as `Concat`. In debug mode panics if the value is not an object id pointing to a `Concat`.
    pub unsafe fn as_concat(self) -> *const Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self.get_object_address() as *const Concat
    }

    /// Get the pointer as `Blob. In debug mode panics if the value is not an object id pointing to a `Blob`.
    pub unsafe fn as_blob(self) -> *const Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self.get_object_address() as *const Blob
    }

    /// Get the pointer as mutable `Blob`.
    pub unsafe fn as_blob_mut(self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self.get_object_address() as *mut Blob
    }

    /// Get the pointer as `Stream`, which is a glorified `Blob`.
    /// In debug mode panics if the value is not an object id pointing to a `Blob`.
    pub unsafe fn as_stream(self) -> *mut Stream {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self.get_object_address() as *mut Stream
    }

    /// Get the pointer as `BigInt`. In debug mode panics if the value is not an object id pointing to a `BigInt`.
    pub unsafe fn as_bigint(self) -> *mut BigInt {
        debug_assert_eq!(self.tag(), TAG_BIGINT);
        self.get_object_address() as *mut BigInt
    }

    pub fn as_tiny(self) -> i32 {
        debug_assert!(self.is_scalar());
        self.0 as i32 >> 1
    }

    // Shortcut version of `value.is_object_id() && value.get_object_address() >= address`.
    // Value is an object id pointing to an object at the unskewed address > 1
    #[inline]
    pub unsafe fn points_to_or_beyond(&self, address: usize) -> bool {
        debug_assert!(address > TRUE_VALUE as usize);
        self.is_object_id() && self.get_object_address() >= address
    }
}

#[inline]
/// Returns whether a raw value is representing an object id. Useful when using `Value::get_raw`.
pub fn is_object_id(value: u32) -> bool {
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
pub const TAG_ARRAY: Tag = 5;
pub const TAG_BITS64: Tag = 7;
pub const TAG_MUTBOX: Tag = 9;
pub const TAG_CLOSURE: Tag = 11;
pub const TAG_SOME: Tag = 13;
pub const TAG_VARIANT: Tag = 15;
pub const TAG_BLOB: Tag = 17;
pub const TAG_FWD_PTR: Tag = 19;
pub const TAG_BITS32: Tag = 21;
pub const TAG_BIGINT: Tag = 23;
pub const TAG_CONCAT: Tag = 25;
pub const TAG_NULL: Tag = 27;
pub const TAG_ONE_WORD_FILLER: Tag = 29;
pub const TAG_FREE_SPACE: Tag = 31;

// Special value to visit only a range of array fields.
// This and all values above it are reserved and mean
// a slice of an array object (i.e. start index) for
// purposes of `visit_pointer_fields`.
// Invariant: the value of this (pseudo-)tag must be
//            higher than all other tags defined above
pub const TAG_ARRAY_SLICE_MIN: Tag = 32;

// Incremental GC Mark Bit.
// Stored in the most significant bit 31 of `raw_id` in the object header:
//
// ┌──────┬───────────────────────────┐
// | Mark |          Id               |
// └──────┴───────────────────────────┘
//  Bit 31        Bits 30..0
//
// Since the object table end address is required to be less than `2**31` (half of the
// heap space), object id values are also less than `2**31`, leaving bit 31 clear.
//
// Used for in-place marking of objects during the incremental GC.
// Note: Using a mark bitmap during incremental GC is not beneficial as garbage objects
// need also be visited during compaction to free their object ids. Therefore, full
// a heap space scan is needed during compaction, while the mark bitmap is optimized
// for skipping unmarked (garbage) objects.

const MARK_BIT_MASK: usize = 1 << 31;

// Common parts of any object. Other object pointers can be coerced into a pointer to this.
#[repr(C)] // See the note at the beginning of this module
pub struct Obj {
    pub tag: Tag,
    /// Object identity including a potential mark bit, used by the incremental GC.
    /// The lower 30 bits encode the skewed pointer to the object's entry in the object table.
    /// The object id supports reverse lookup of the object in the object table.
    /// The mark bit is used during incremental GC (young and old generation).
    mark_and_id: usize,
}

impl Obj {
    pub unsafe fn initialize_id(&mut self, object_id: Value) {
        debug_assert!(
            OBJECT_TABLE == null_mut() || object_id.get_raw() as usize & MARK_BIT_MASK == 0
        );
        self.mark_and_id = object_id.get_raw() as usize;
    }

    pub unsafe fn object_id(self: *const Self) -> Value {
        if OBJECT_TABLE != null_mut() {
            Value::from_raw(((*self).mark_and_id & !MARK_BIT_MASK) as u32)
        } else {
            Value::from_raw(skew(self as usize) as u32)
        }
    }

    pub unsafe fn is_marked(self: *const Self) -> bool {
        debug_assert_ne!(OBJECT_TABLE, null_mut());
        (*self).mark_and_id & MARK_BIT_MASK != 0
    }

    pub unsafe fn mark(self: *mut Self) {
        debug_assert_ne!(OBJECT_TABLE, null_mut());
        (*self).mark_and_id |= MARK_BIT_MASK;
    }

    pub unsafe fn unmark(self: *mut Self) {
        debug_assert_ne!(OBJECT_TABLE, null_mut());
        (*self).mark_and_id &= !MARK_BIT_MASK;
    }

    pub unsafe fn tag(self: *const Self) -> Tag {
        (*self).tag
    }

    pub unsafe fn as_blob(self: *mut Self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
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

    /// Write a pointer value to an array element. Uses a post-update barrier.
    pub unsafe fn set_pointer<M: Memory>(self: *mut Self, idx: u32, value: Value, mem: &mut M) {
        debug_assert!(value.is_object_id());
        let slot_addr = self.element_address(idx);
        if using_incremental_barrier() {
            write_with_barrier(mem, slot_addr as *mut Value, value);
        } else {
            *(slot_addr as *mut Value) = value;
            if using_generational_barrier() {
                generational_write_barrier(mem, slot_addr as usize);
            }
        }
    }

    /// Write a scalar value to an array element. No need for a write barrier.
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

#[repr(C)] // See the note at the beginning of this module
pub struct Stream {
    pub header: Blob,
    pub padding: u32, // The insertion of the object id in the header implies 1 word padding to 64-bit.
    pub ptr64: u64,
    pub start64: u64,
    pub limit64: u64,
    pub outputter: fn(*mut Self, *const u8, Bytes<u32>) -> (),
    pub filled: Bytes<u32>, // cache data follows ..
}

impl Stream {
    pub unsafe fn as_blob_mut(self: *mut Self) -> *mut Blob {
        self as *mut Blob
    }
}

/// Only used by the copying GC.
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

    pub unsafe fn from_payload(ptr: *mut mp_digit) -> *mut Self {
        (ptr as *mut u32).sub(size_of::<BigInt>().as_usize()) as *mut BigInt
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

/// Determines whether an heap block with this tag has a regular header that contains an object id.
/// Returns `false` for `OneWordFiller`, `FwdPtr`, and `FreeSpace` tags whose blocks do not have
/// a regular object header.
pub(crate) unsafe fn has_object_header(tag: Tag) -> bool {
    tag != TAG_FWD_PTR && tag != TAG_ONE_WORD_FILLER && tag != TAG_FREE_SPACE
}

/// Returns the heap block size in words.
/// Handles both objects with header that stores the object id and
/// special blocks such as `OneWordFiller`, `FwdPtr`, and `FreeSpace`
/// that do not have a regular header with object id.
pub(crate) unsafe fn block_size(address: usize) -> Words<u32> {
    let tag = *(address as *mut Tag);
    match tag {
        TAG_OBJECT => {
            let object = address as *mut Object;
            let size = object.size();
            size_of::<Object>() + Words(size)
        }

        TAG_OBJ_IND => size_of::<ObjInd>(),

        TAG_ARRAY => {
            let array = address as *mut Array;
            let size = array.len();
            size_of::<Array>() + Words(size)
        }

        TAG_BITS64 => size_of::<Bits64>(),

        TAG_MUTBOX => size_of::<MutBox>(),

        TAG_CLOSURE => {
            let closure = address as *mut Closure;
            let size = closure.size();
            size_of::<Closure>() + Words(size)
        }

        TAG_SOME => size_of::<Some>(),

        TAG_VARIANT => size_of::<Variant>(),

        TAG_BLOB => {
            let blob = address as *mut Blob;
            size_of::<Blob>() + blob.len().to_words()
        }

        TAG_FWD_PTR => {
            rts_trap_with("block_size: forwarding pointer");
        }

        TAG_BITS32 => size_of::<Bits32>(),

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

        _ => {
            rts_trap_with("block_size: invalid object tag");
        }
    }
}
