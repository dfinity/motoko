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

use crate::tommath_bindings::{mp_digit, mp_int};
use core::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};

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

/// A value in a heap slot
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
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
        // TODO: Update rustc to enable assertion
        // debug_assert_eq!(ptr & 0b1, 0b0);
        Value(skew(ptr) as u32)
    }

    /// Create a value from a scalar
    pub const fn from_scalar(value: u32) -> Self {
        // TODO: Update rustc to enable assertion
        // debug_assert_eq!(value >> 31, 0);
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
        if self.0 & 0b1 == 0b1 {
            PtrOrScalar::Ptr(unskew(self.0 as usize))
        } else {
            PtrOrScalar::Scalar(self.0 >> 1)
        }
    }

    /// Get the raw value
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

    /// Is the value a pointer to dynamic heap?
    pub fn is_ptr_to_dynamic_heap(&self) -> bool {
        match self.get() {
            PtrOrScalar::Ptr(ptr) => !unsafe { (ptr as *mut Obj).is_static() },
            PtrOrScalar::Scalar(_) => false,
        }
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

    /// Get the object tag. In debug mode panics if the value is not a pointer.
    pub unsafe fn tag(self) -> Tag {
        debug_assert!(self.get().is_ptr());
        (self.get_ptr() as *mut Obj).tag()
    }

    /// Get the pointer as `Obj`. In debug mode panics if the value is not a pointer.
    pub unsafe fn as_obj(self) -> *mut Obj {
        debug_assert!(self.get().is_ptr());
        self.get_ptr() as *mut Obj
    }

    /// Get the pointer as `Array`. In debug mode panics if the value is not a pointer or the
    /// pointed object is not an `Array`.
    pub unsafe fn as_array(self) -> *mut Array {
        debug_assert_eq!(self.tag(), TAG_ARRAY);
        self.get_ptr() as *mut Array
    }

    /// Get the pointer as `Concat`. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `Concat`.
    pub unsafe fn as_concat(self) -> *mut Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self.get_ptr() as *mut Concat
    }

    /// Get the pointer as `Blob`. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `Blob`.
    pub unsafe fn as_blob(self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self.get_ptr() as *mut Blob
    }

    /// Get the pointer as `BigInt`. In debug mode panics if the value is not a pointer or the
    /// pointed object is not a `BigInt`.
    pub unsafe fn as_bigint(self) -> *mut BigInt {
        debug_assert_eq!(self.tag(), TAG_BIGINT);
        self.get_ptr() as *mut BigInt
    }

    pub fn as_tiny(self) -> i32 {
        debug_assert!(self.is_scalar());
        self.0 as i32 >> 1
    }
}

/// Returns whether a raw value is representing a pointer. Useful when using `Value::get_raw`.
pub fn is_ptr(value: u32) -> bool {
    value & 0b1 == 0b1
}

pub const fn skew(ptr: usize) -> usize {
    ptr.wrapping_sub(1)
}

pub const fn unskew(value: usize) -> usize {
    value.wrapping_add(1)
}

// NOTE: We don't create an enum for tags as we can never assume to do exhaustive pattern match on
// tags, because of heap corruptions and other bugs (in the code generator or RTS, or maybe because
// of an unsafe API usage).
pub type Tag = u8;

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

// A bitmap for GC metadata. Currently only holds one bit for large objects.
pub struct GcMetadata(u8);

const STATIC_BIT_MASK: u8 = 0b1;
const LARGE_BIT_MASK: u8 = 0b10;

impl GcMetadata {
    /// Is the object static?
    pub fn is_static(&self) -> bool {
        self.check_bitset_sanity();
        self.0 & STATIC_BIT_MASK != 0
    }

    /// Set the "is static" bit. Static objects are normally allocated in compile time, this method
    /// is only used in tests.
    pub fn set_static(&mut self) {
        self.check_bitset_sanity();
        self.0 |= STATIC_BIT_MASK
    }

    /// Is the object large?
    fn is_large(&self) -> bool {
        self.check_bitset_sanity();
        self.0 & LARGE_BIT_MASK != 0
    }

    /// Set the "is large" bit
    fn set_large(&mut self) {
        self.check_bitset_sanity();
        self.0 |= LARGE_BIT_MASK
    }

    fn check_bitset_sanity(&self) {
        debug_assert!(self.0 >> 2 == 0);
    }
}

// Common parts of any object. Other object pointers can be coerced into a pointer to this.
#[repr(C)] // See the note at the beginning of this module
pub struct Obj {
    pub tag: Tag,
    pub gc_metadata: GcMetadata,
    // Add padding to make it 1 word
    padding: u16,
}

impl Obj {
    pub unsafe fn as_word(self: *mut Self) -> u32 {
        *(self as *const u32)
    }

    pub unsafe fn set_header_word(self: *mut Self, header: u32) {
        *(self as *mut u32) = header;
    }

    pub unsafe fn from_header_word(header: u32) -> Obj {
        Obj {
            tag: header as u8,
            gc_metadata: GcMetadata((header >> 8) as u8),
            padding: (header >> 16) as u16,
        }
    }

    pub unsafe fn tag(self: *mut Self) -> Tag {
        (*self).gc_metadata.check_bitset_sanity();
        (*self).tag
    }

    pub unsafe fn set_tag(self: *mut Self, tag: Tag) {
        (*self).tag = tag
    }

    pub unsafe fn as_blob(self: *mut Self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self as *mut Blob
    }

    pub unsafe fn as_concat(self: *mut Self) -> *mut Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self as *mut Concat
    }

    /// Returns whether this is a large object
    pub unsafe fn is_large(self: *mut Self) -> bool {
        (*self).gc_metadata.is_large()
    }

    /// Set the "large" bit
    pub unsafe fn set_large(self: *mut Self) {
        (*self).gc_metadata.set_large()
    }

    /// Returns whether this is a static object
    pub unsafe fn is_static(self: *mut Self) -> bool {
        (*self).gc_metadata.is_static()
    }

    /// Set the "is static" bit. Static objects are normally allocated in compile time, this method
    /// is only used in tests.
    pub unsafe fn set_static(self: *mut Self) {
        (*self).gc_metadata.set_static()
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
    pub unsafe fn set_tag(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_tag(TAG_ARRAY);
    }

    pub unsafe fn payload_addr(self: *mut Self) -> *mut Value {
        self.offset(1) as *mut Value // skip array header
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> Value {
        debug_assert!(self.len() > idx);
        let slot_addr = self.payload_addr() as usize + (idx * WORD_SIZE) as usize;
        *(slot_addr as *const Value)
    }

    pub unsafe fn set(self: *mut Self, idx: u32, ptr: Value) {
        debug_assert!(self.len() > idx);
        let slot_addr = self.payload_addr() as usize + (idx * WORD_SIZE) as usize;
        *(slot_addr as *mut Value) = ptr;
    }

    pub unsafe fn len(self: *mut Self) -> u32 {
        (*self).len
    }

    pub unsafe fn set_len(self: *mut Self, len: u32) {
        (*self).len = len;
    }

    // Used in tests
    pub unsafe fn set_static(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_static()
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
    pub unsafe fn set_tag(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_tag(TAG_BLOB);
    }

    pub unsafe fn payload_addr(self: *mut Self) -> *mut u8 {
        self.add(1) as *mut u8 // skip closure header
    }

    pub unsafe fn len(self: *mut Self) -> Bytes<u32> {
        (*self).len
    }

    pub unsafe fn set_len(self: *mut Self, len: Bytes<u32>) {
        (*self).len = len;
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> u8 {
        *self.payload_addr().add(idx as usize)
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
            filler.set_tag();
        } else if slop != Words(0) {
            let filler =
                (self.payload_addr() as *mut u32).add(new_len_words.as_usize()) as *mut FreeSpace;
            filler.set_tag();
            (*filler).words = slop - Words(1);
        }

        (*self).len = new_len;
    }
}

/// A forwarding pointer placed by the GC in place of an evacuated object.
#[repr(C)] // See the note at the beginning of this module
pub struct FwdPtr {
    pub header: Obj,
    pub fwd: Value,
}

impl FwdPtr {
    pub unsafe fn set_tag(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_tag(TAG_FWD_PTR);
    }

    pub unsafe fn set_forwarding(self: *mut Self, addr: usize) {
        (*self).fwd = Value::from_ptr(addr);
    }
}

#[repr(C)] // See the note at the beginning of this module
pub struct BigInt {
    pub header: Obj,
    /// The data following now must describe is the `mp_int` struct.
    /// The data pointer (mp_int.dp) is irrelevant, and will be changed to point to
    /// the data within this object before it is used.
    /// (NB: If we have a non-moving GC, we can make this an invaiant)
    pub mp_int: mp_int,
    // data follows ..
}

impl BigInt {
    pub unsafe fn set_tag(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_tag(TAG_BIGINT);
    }

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

impl MutBox {
    pub unsafe fn set_tag(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_tag(TAG_MUTBOX);
    }

    pub unsafe fn set_field(self: *mut Self, field: Value) {
        (*self).field = field;
    }

    // Used in tests
    pub unsafe fn set_static(self: *mut Self) {
        (&mut (*self).header as *mut Obj).set_static()
    }
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
    pub unsafe fn set_tag(self: *mut Self) {
        ((&mut (*self).header) as *mut Obj).set_tag(TAG_CONCAT);
    }

    pub unsafe fn text1(self: *mut Self) -> Value {
        (*self).text1
    }

    pub unsafe fn text2(self: *mut Self) -> Value {
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
    pub header: Obj,
}

impl OneWordFiller {
    unsafe fn set_tag(self: *mut Self) {
        ((&mut (*self).header) as *mut Obj).set_tag(TAG_ONE_WORD_FILLER);
    }
}

/// Marks arbitrary sized emtpy space in heap
#[repr(C)] // See the note at the beginning of this module
pub struct FreeSpace {
    pub header: Obj,
    pub words: Words<u32>,
}

impl FreeSpace {
    unsafe fn set_tag(self: *mut Self) {
        ((&mut (*self).header) as *mut Obj).set_tag(TAG_FREE_SPACE);
    }

    /// Size of the free space (includes object header)
    pub unsafe fn size(self: *mut Self) -> Words<u32> {
        (*self).words + size_of::<Obj>()
    }
}

/// Returns object size in words
pub unsafe fn object_size(obj: usize) -> Words<u32> {
    let obj_ = obj as *mut Obj;
    object_size_(obj, obj_.tag())
}

pub(crate) unsafe fn object_size_(obj: usize, tag: Tag) -> Words<u32> {
    let obj = obj as *mut Obj;
    match tag {
        TAG_OBJECT => {
            let object = obj as *mut Object;
            let size = object.size();
            size_of::<Object>() + Words(size)
        }

        TAG_OBJ_IND => size_of::<ObjInd>(),

        TAG_ARRAY => {
            let array = obj as *mut Array;
            let size = array.len();
            size_of::<Array>() + Words(size)
        }

        TAG_BITS64 => size_of::<Bits64>(),

        TAG_MUTBOX => size_of::<MutBox>(),

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let size = closure.size();
            size_of::<Closure>() + Words(size)
        }

        TAG_SOME => size_of::<Some>(),

        TAG_VARIANT => size_of::<Variant>(),

        TAG_BLOB => {
            let blob = obj as *mut Blob;
            size_of::<Blob>() + blob.len().to_words()
        }

        TAG_FWD_PTR => {
            rts_trap_with("object_size: forwarding pointer");
        }

        TAG_BITS32 => size_of::<Bits32>(),

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            size_of::<BigInt>() + bigint.len().to_words()
        }

        TAG_CONCAT => size_of::<Concat>(),

        TAG_NULL => size_of::<Null>(),

        TAG_ONE_WORD_FILLER => size_of::<OneWordFiller>(),

        TAG_FREE_SPACE => {
            let free_space = obj as *mut FreeSpace;
            free_space.size()
        }

        _ => {
            rts_trap_with("object_size: invalid object tag");
        }
    }
}
