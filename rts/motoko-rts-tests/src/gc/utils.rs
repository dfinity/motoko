use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use motoko_rts::{
    constants::ADDRESS_ALIGNMENT,
    types::{Value, Words},
};

/// A unique object index, used in heap descriptions.
///
/// These are written as scalar values in object payloads, so they can be at most 31 bits. Larger
/// values will cause test failure in `make_scalar` below.
pub type ObjectIdx = u32;

/// Same as RTS `WORD_SIZE`, but `usize`
pub const WORD_SIZE: usize = motoko_rts::constants::WORD_SIZE as usize;

/// Read a little-endian (Wasm) word from given offset
pub fn read_word(heap: &[u8], offset: usize) -> u32 {
    (&heap[offset..]).read_u32::<LE>().unwrap()
}

/// Read a little-endian (Wasm) 64-bit word from given offset
pub fn read_word64(heap: &[u8], offset: usize) -> u64 {
    (&heap[offset..]).read_u64::<LE>().unwrap()
}

/// Write a little-endian (Wasm) word to given offset
pub fn write_word(heap: &mut [u8], offset: usize, word: u32) {
    (&mut heap[offset..]).write_u32::<LE>(word).unwrap()
}

/// Make a scalar value to be used in heap object payload
pub fn make_scalar(value: u32) -> u32 {
    // Scalar values can be at most 31 bits
    assert_eq!(value >> 31, 0);
    value << 1
}

/// Inverse of `make_scalar`
pub fn get_scalar_value(scalar: u32) -> u32 {
    assert_eq!(scalar & 0b1, 0);
    scalar >> 1
}

/// Make a pointer value to be used in heap object payload
pub fn make_pointer(address: usize) -> u32 {
    Value::from_ptr(address).get_raw()
}

/// Inverse of `make_pointer`
pub fn unskew_pointer(skewed_ptr: u32) -> usize {
    Value::from_raw(skewed_ptr).get_ptr()
}

pub fn round_to_alignment(size: Words<usize>) -> usize {
    let alignment = ADDRESS_ALIGNMENT.to_bytes().as_usize();
    (size.to_bytes().as_usize() + alignment - 1) / alignment * alignment
}

pub fn check_alignment(address: usize) {
    assert_eq!(address % ADDRESS_ALIGNMENT.to_bytes().as_usize(), 0);
}
