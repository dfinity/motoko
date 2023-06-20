use crate::types::{Bytes, Words};

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: usize = 8;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<usize> = Bytes(64 * 1024);

/// Wasm heap size in words. Note that `to_bytes` on this value will overflow as it is `usize::MAX + 1`.
pub const WASM_HEAP_SIZE: Words<usize> = Words(usize::MAX / WORD_SIZE + 1);
