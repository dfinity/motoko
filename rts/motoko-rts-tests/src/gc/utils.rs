use byteorder::{ReadBytesExt, WriteBytesExt, LE};

/// A unique object index, used in heap descriptions.
///
/// These are written as scalar values in object payloads, so they can be at most 31 bits. Larger
/// values will cause test failure in `make_scalar` below.
pub type ObjectIdx = u32;

/// Same as RTS `WORD_SIZE`, but `usize`
pub const WORD_SIZE: usize = motoko_rts::constants::WORD_SIZE as usize;

// Max allowed size for the mark stack in mark-compact GC tests
pub const MAX_MARK_STACK_SIZE: usize = 100;

/// Enum for the GC implementations. GC functions are generic so we can't put them into arrays or
/// other data types, we use this type instead.
#[derive(Debug, Clone, Copy)]
pub enum GC {
    Copying,
    MarkCompact,
}

pub static GC_IMPLS: [GC; 2] = [GC::Copying, GC::MarkCompact];

/// Read a little-endian (Wasm) word from given offset
pub fn read_word(heap: &[u8], offset: usize) -> u32 {
    (&heap[offset..]).read_u32::<LE>().unwrap()
}

/// Write a little-endian (Wasm) word to given offset
pub fn write_word(heap: &mut [u8], offset: usize, word: u32) {
    (&mut heap[offset..]).write_u32::<LE>(word).unwrap()
}

/// Read a single byte from given offset
pub fn read_byte(heap: &[u8], offset: usize) -> u8 {
    heap[offset]
}

/// Write a single byte to given offset
pub fn write_byte(heap: &mut [u8], offset: usize, byte: u8) {
    heap[offset] = byte
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
pub fn make_pointer(addr: u32) -> u32 {
    addr.wrapping_sub(1)
}

/// Inverse of `make_pointer`
pub fn unskew_pointer(skewed_ptr: u32) -> u32 {
    skewed_ptr.wrapping_add(1)
}
