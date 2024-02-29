use crate::types::Bytes;

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: usize = 8;

/// Maximum Motoko array size 2^29 (inclusive)
/// NB: Must agree with Arr.max_array_size in compile.ml.
pub const MAX_ARRAY_SIZE: u32 = 1 << 29;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<usize> = Bytes(64 * KB);

/// Byte constants
pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;
