pub const WORD_SIZE: u32 = 4;

pub fn words_to_bytes(words: Words<u32>) -> Bytes<u32> {
    Bytes(words.0 * WORD_SIZE)
}

// Rounds up
pub fn bytes_to_words(bytes: Bytes<u32>) -> Words<u32> {
    // Rust issue for adding ceiling_div: https://github.com/rust-lang/rfcs/issues/2844
    Words((bytes.0 + WORD_SIZE - 1) / WORD_SIZE)
}

/// The unit "words": `Words(123u32)` means 123 words.
#[repr(C)]
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Words<A>(pub A);

/// The unit "bytes": `Bytes(123u32)` means 123 bytes.
#[repr(C)]
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Bytes<A>(pub A);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SkewedPtr(pub usize);

impl SkewedPtr {
    pub fn unskew(self) -> usize {
        self.0.wrapping_add(1)
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
pub const TAG_INDIRECTION: Tag = 11;
pub const TAG_BITS32: Tag = 12;
pub const TAG_BIGINT: Tag = 13;
pub const TAG_CONCAT: Tag = 14;

// Common parts of any object. Other object pointers can be coerced into a pointer to this.
#[repr(C)]
pub struct Obj {
    pub tag: Tag,
}

#[repr(C)]
#[rustfmt::skip]
pub struct Array {
    pub header: Obj,
    pub len: u32, // number of elements

    // Array elements follow, each u32 sized. We can't have variable-sized structs in Rust so we
    // can't add a field here for the elements.
    // https://doc.rust-lang.org/nomicon/exotic-sizes.html
}

pub(crate) unsafe fn array_get(array: *const Array, idx: u32) -> u32 {
    let slot_addr = array.offset(1) as usize + (idx * WORD_SIZE) as usize;
    *(slot_addr as *const u32)
}

#[repr(C)]
pub struct Object {
    pub header: Obj,
    pub size: u32,
    pub hash_ptr: u32, // TODO: Not sure how this is used, we don't scavenge this field in GC
                       // other stuff follows, but we don't need them currently
}

#[repr(C)]
pub struct ObjInd {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(C)]
pub struct Closure {
    pub header: Obj,
    pub funid: u32,
    pub size: u32, // number of elements
                   // other stuff follows ...
}

#[repr(C)]
pub struct Blob {
    pub header: Obj,
    pub len: Bytes<u32>,
    // data follows ..
}

// aka. a forwarding pointer
#[repr(C)]
pub struct Indirection {
    pub header: Obj,
    pub fwd: SkewedPtr,
}

#[repr(C)]
pub struct BigInt {
    pub header: Obj,
    pub size: u32,
    pub alloc: u32, // TODO: Not sure what this is
    // Unskewed pointer to a blob payload. data_ptr - 2 (words) gives us the blob header.
    pub data_ptr: usize,
}

#[repr(C)]
pub struct MutBox {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(C)]
pub struct Some {
    pub header: Obj,
    pub field: SkewedPtr,
}

#[repr(C)]
pub struct Variant {
    pub header: Obj,
    pub tag: u32,
    pub field: SkewedPtr,
}

#[repr(C)]
pub struct Concat {
    pub header: Obj,
    pub n_bytes: u32,
    pub text1: SkewedPtr,
    pub text2: SkewedPtr,
}
