use crate::types::{Bytes, Words};

/// Size of object fields, vanilla values.
/// To save memory, the word size remains 32 bit in the 64-bit main memory.
/// Pointers are represented in a compact format, capable to address 32 GB.
pub const WORD_SIZE: usize = 4;

/// Maximum Motoko array size 2^29 (inclusive)
/// NB: Must agree with Arr.max_array_size in compile.ml.
pub const MAX_ARRAY_SIZE: usize = 1 << 29;

/// Wasm page size (64 KiB) in bytes
pub const WASM_PAGE_SIZE: Bytes<usize> = Bytes(64 * 1024);

/// Objects are aligned to 4 words = 16 bytes.
/// This is to extend the addressable space for the 32-bit compact pointers.
pub const ADDRESS_ALIGNMENT: Words<usize> = Words(4);

/// By aligning addresses to 16 bytes, an address has its 4 lowest bit set to zero.
/// One bit is used for pointer skewing (tagging), remaining 3 bits for extending the address space by factor 8.
/// The maximum address space in compact 32-bit representation is thus 8 * 4GB = 32GB.
pub const MAX_MEMORY_SIZE: Bytes<usize> =
    Bytes(4 * GB * (ADDRESS_ALIGNMENT.to_bytes().as_usize() / 2));

/// Byte constants
pub const KB: usize = 1024;
pub const MB: usize = 1024 * KB;
pub const GB: usize = 1024 * MB;
