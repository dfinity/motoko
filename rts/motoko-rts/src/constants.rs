use crate::types::{Bytes, Words};

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: usize = 8;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<usize> = Bytes(64 * KB);

/// Byte constants
pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;
