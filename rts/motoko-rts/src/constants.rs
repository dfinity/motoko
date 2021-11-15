use crate::types::{Bytes, Words};

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: u32 = 4;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<u32> = Bytes(64 * 1024);

/// Wasm heap size (4 GiB) in words. Note that `to_bytes` on this value will overflow as 4 GiB in
/// bytes is `u32::MAX + 1`.
pub const WASM_HEAP_SIZE: Words<u32> = Words(1024 * 1024 * 1024);
