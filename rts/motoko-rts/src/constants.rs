use crate::types::Bytes;

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: usize = 8;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<usize> = Bytes(64 * KB);

/// Byte constants
pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;

// The optimized array iterator requires array lengths to fit in signed compact numbers.
// See `compile.ml`, `GetPastArrayOffset`.
// Two bits reserved: Two for Int tag (0b10L) and one for the sign bit.
pub const MAX_ARRAY_LENGTH_FOR_ITERATOR: usize = 1 << (usize::BITS as usize - 3);
