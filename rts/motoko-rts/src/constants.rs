use crate::types::{Bytes, Words};

/// Wasm word size. RTS only works correctly on platforms with this word size.
pub const WORD_SIZE: u32 = 4;

/// Maximum Motoko array size 2^29 (inclusive)
/// NB: Must agree with Arr.max_size in compile.ml.
pub const MAX_ARRAY_SIZE: u32 = 1 << 29;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<u32> = Bytes(64 * 1024);

/// Wasm heap size (4 GiB) in words. Note that `to_bytes` on this value will overflow as 4 GiB in
/// bytes is `u32::MAX + 1`.
pub const WASM_HEAP_SIZE: Words<u32> = Words(1024 * 1024 * 1024);

/// Wasm memory size (4 GiB) in bytes. Note: Represented as `u64` in order not to overflow.
pub const WASM_MEMORY_BYTE_SIZE: Bytes<u64> = Bytes(4 * 1024 * 1024 * 1024);

/// Byte constants
pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;
