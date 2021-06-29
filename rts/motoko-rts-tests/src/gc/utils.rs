use byteorder::{ReadBytesExt, WriteBytesExt, LE};

/// A unique object index, used in heap descriptions
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
