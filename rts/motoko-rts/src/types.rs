use core::ops::{Add, AddAssign, Sub, SubAssign};

use crate::rts_trap_with;

pub fn size_of<T>() -> Words<u32> {
    Bytes(::core::mem::size_of::<T>() as u32).to_words()
}

pub const WORD_SIZE: u32 = 4;

/// The unit "words": `Words(123u32)` means 123 words.
#[repr(transparent)]
#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Words<A>(pub A);

impl Words<u32> {
    pub fn to_bytes(self) -> Bytes<u32> {
        Bytes(self.0 * WORD_SIZE)
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

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SkewedPtr(pub usize);

impl SkewedPtr {
    pub unsafe fn tag(self) -> Tag {
        (self.unskew() as *mut Obj).tag()
    }

    pub fn unskew(self) -> usize {
        self.0.wrapping_add(1)
    }

    /// This is for sanity checking: a skewed pointer can't be a tagged scalar
    pub fn is_tagged_scalar(&self) -> bool {
        self.0 & 0b1 == 0
    }

    pub unsafe fn as_obj(self) -> *mut Obj {
        self.unskew() as *mut Obj
    }

    pub unsafe fn as_array(self) -> *mut Array {
        debug_assert_eq!(self.tag(), TAG_ARRAY);
        self.unskew() as *mut Array
    }

    pub unsafe fn as_concat(self) -> *mut Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self.unskew() as *mut Concat
    }

    pub unsafe fn as_blob(self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self.unskew() as *mut Blob
    }

    pub unsafe fn as_bigint(self) -> *mut BigInt {
        debug_assert_eq!(self.tag(), TAG_BIGINT);
        self.unskew() as *mut BigInt
    }
}

pub fn skew(ptr: usize) -> SkewedPtr {
    SkewedPtr(ptr.wrapping_sub(1))
}

// NOTE: We don't create an enum for tags as we can never assume to do exhaustive pattern match on
// tags, because of heap corruptions and other bugs (in the code generator or RTS, or maybe because
// of an unsafe API usage).
pub type Tag = u32;

pub const TAG_OBJECT: Tag = 1;
pub const TAG_OBJ_IND: Tag = 2;
pub const TAG_ARRAY: Tag = 3;
pub const TAG_BITS64: Tag = 5;
pub const TAG_MUTBOX: Tag = 6;
pub const TAG_CLOSURE: Tag = 7;
pub const TAG_SOME: Tag = 8;
pub const TAG_VARIANT: Tag = 9;
pub const TAG_BLOB: Tag = 10;
pub const TAG_FWD_PTR: Tag = 11;
pub const TAG_BITS32: Tag = 12;
pub const TAG_BIGINT: Tag = 13;
pub const TAG_CONCAT: Tag = 14;
pub const TAG_NULL: Tag = 15;

// Common parts of any object. Other object pointers can be coerced into a pointer to this.
#[repr(packed)]
pub struct Obj {
    pub tag: Tag,
}

impl Obj {
    pub unsafe fn tag(self: *mut Self) -> Tag {
        (*self).tag
    }

    pub unsafe fn as_blob(self: *mut Self) -> *mut Blob {
        debug_assert_eq!(self.tag(), TAG_BLOB);
        self as *mut Blob
    }

    pub unsafe fn as_concat(self: *mut Self) -> *mut Concat {
        debug_assert_eq!(self.tag(), TAG_CONCAT);
        self as *mut Concat
    }
}

#[repr(packed)]
#[rustfmt::skip]
pub struct Array {
    pub header: Obj,
    pub len: u32, // number of elements

    // Array elements follow, each u32 sized. We can't have variable-sized structs in Rust so we
    // can't add a field here for the elements.
    // https://doc.rust-lang.org/nomicon/exotic-sizes.html
}

impl Array {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut SkewedPtr {
        self.offset(1) as *mut SkewedPtr // skip array header
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> SkewedPtr {
        let slot_addr = self.payload_addr() as usize + (idx * WORD_SIZE) as usize;
        *(slot_addr as *const SkewedPtr)
    }

    pub unsafe fn set(self: *mut Self, idx: u32, ptr: SkewedPtr) {
        let slot_addr = self.payload_addr() as usize + (idx * WORD_SIZE) as usize;
        *(slot_addr as *mut SkewedPtr) = ptr;
    }

    pub unsafe fn len(self: *mut Self) -> u32 {
        (*self).len
    }
}

#[repr(packed)]
pub struct Object {
    pub header: Obj,
    pub size: u32,     // Number of elements
    pub hash_ptr: u32, // Pointer to static information about object field labels. Not important for GC (does not contain pointers).
}

impl Object {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut SkewedPtr {
        self.add(1) as *mut SkewedPtr // skip object header
    }

    pub(crate) unsafe fn size(self: *mut Self) -> u32 {
        (*self).size
    }

    pub(crate) unsafe fn get(self: *mut Self, idx: u32) -> SkewedPtr {
        *self.payload_addr().add(idx as usize)
    }
}

#[repr(packed)]
pub struct ObjInd {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(packed)]
pub struct Closure {
    pub header: Obj,
    pub funid: u32,
    pub size: u32, // number of elements
                   // other stuff follows ...
}

impl Closure {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut SkewedPtr {
        self.offset(1) as *mut SkewedPtr // skip closure header
    }

    pub(crate) unsafe fn size(self: *mut Self) -> u32 {
        (*self).size
    }

    pub(crate) unsafe fn get(self: *mut Self, idx: u32) -> SkewedPtr {
        *self.payload_addr().add(idx as usize)
    }
}

#[repr(packed)]
pub struct Blob {
    pub header: Obj,
    pub len: Bytes<u32>,
    // data follows ..
}

impl Blob {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut u8 {
        self.add(1) as *mut u8 // skip closure header
    }

    pub unsafe fn len(self: *mut Self) -> Bytes<u32> {
        (*self).len
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> u8 {
        *self.payload_addr().add(idx as usize)
    }

    pub unsafe fn set(self: *mut Self, idx: u32, byte: u8) {
        *self.payload_addr().add(idx as usize) = byte;
    }
}

/// A forwarding pointer placed by the GC in place of an evacuated object.
#[repr(packed)]
pub struct FwdPtr {
    pub header: Obj,
    pub fwd: SkewedPtr,
}

#[repr(packed)]
pub struct BigInt {
    pub header: Obj,
    /// The data following now must describe is the `mp_int` struct. The data pointer (`mp_int.dp`)
    /// is an unskewed pointer to a blob *header*. `mp_int` functions require `mp_int.dp` to point
    /// to a buffer, so before calling `mp_int` functions we adjust the field, in `mp_int_ptr`, and
    /// restore the field with `restore_mp_int_ptr` afterwards.
    pub mp_int: crate::tommath_bindings::mp_int,
}

impl BigInt {
    /// TODO document
    pub unsafe fn blob_field(self: *mut BigInt) -> *mut SkewedPtr {
        &mut (*self).mp_int.dp as *mut _ as *mut _
    }

    /// Adjusts `mp_int_ptr` data pointer so that it points to the `Blob`s payload (instead of
    /// header) then returns the pointer to the `mp_int` struct. Make sure to call
    /// `restore_mp_int_ptr` afterwards.
    unsafe fn mp_int_ptr(self: *mut BigInt) -> *mut crate::tommath_bindings::mp_int {
        if (*self).mp_int.dp != core::ptr::null_mut() {
            debug_assert_eq!(
                (SkewedPtr((*self).mp_int.dp as usize).unskew() as *mut Obj).tag(),
                TAG_BLOB
            );
        }

        (*self).mp_int.dp = ((*self.blob_field()).unskew() as *mut Blob).add(1) as *mut _;
        &mut (*self).mp_int
    }

    /// Reverses the adjustment in `mp_int_ptr`
    unsafe fn restore_mp_int_ptr(self: *mut BigInt) {
        let blob_header_ptr = ((*self).mp_int.dp as *mut Blob).sub(1);
        debug_assert_eq!((*blob_header_ptr).header.tag, TAG_BLOB);
        (*self).mp_int.dp = skew(blob_header_ptr as usize).0 as *mut _;
    }

    pub unsafe fn with_mp_int_ptr<F, A>(self: *mut BigInt, mut f: F) -> A
    where
        F: FnMut(*mut crate::tommath_bindings::mp_int) -> A,
    {
        let mp_int_ptr = self.mp_int_ptr();
        let ret = f(mp_int_ptr);
        self.restore_mp_int_ptr();
        ret
    }
}

#[repr(packed)]
pub struct MutBox {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(packed)]
pub struct Some {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(packed)]
pub struct Variant {
    pub header: Obj,
    pub tag: u32,
    pub field: SkewedPtr,
}

#[repr(packed)]
pub struct Concat {
    pub header: Obj,
    pub n_bytes: Bytes<u32>,
    pub text1: SkewedPtr,
    pub text2: SkewedPtr,
}

impl Concat {
    pub unsafe fn text1(self: *mut Self) -> SkewedPtr {
        (*self).text1
    }

    pub unsafe fn text2(self: *mut Self) -> SkewedPtr {
        (*self).text2
    }
}

#[repr(packed)]
pub struct Null {
    pub header: Obj,
}

#[repr(packed)]
pub struct Bits64 {
    pub header: Obj,
    pub bits: u64,
}

#[repr(packed)]
pub struct Bits32 {
    pub header: Obj,
    pub bits: u32,
}

/// Returns object size in words
pub(crate) unsafe fn object_size(obj: usize) -> Words<u32> {
    let obj = obj as *mut Obj;
    match obj.tag() {
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

        TAG_BIGINT => size_of::<BigInt>(),

        TAG_CONCAT => size_of::<Concat>(),

        TAG_NULL => size_of::<Null>(),

        0 => {
            // This can happens when we shrink a blob in principal id functions. The slop between
            // new size and old size is filled with zeros.
            Words(1)
        }

        _ => {
            rts_trap_with("object_size: invalid object tag");
        }
    }
}
