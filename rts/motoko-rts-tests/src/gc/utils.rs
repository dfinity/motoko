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
