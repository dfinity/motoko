//! Resolution of name hashes for debugging or introspection purposes.
//!
//! Currently only used for stable function names, to yield better error messages
//! for missing or incompatible stable functions.
//!
//! Can be extended to also cover field and option names, for e.g. data visualizer.

use crate::algorithms::SortedArray;
use crate::persistence::load_name_table;
use crate::types::{block_size, Blob, Value};
use alloc::string::{String, ToString};
use core::slice::from_raw_parts;
use core::str::from_utf8;

// Not using `u32` or `u64` to avoid implicit padding issues in `NameRecord`.
type NameHash = usize;

#[repr(C)]
struct NameRecord {
    hash: NameHash,
    // Offset in text section.
    offset: usize,
    // Length in text section.
    length: usize,
}

#[repr(C)]
pub struct NameTable {
    header: Blob,
    // Number of hash entries.
    size: usize,
    // Series of `NameRecord` of length `size`, sorted by `hash`.
    // UTF8 text section, texts are looked up by `offset` and `length`.
}

impl NameTable {
    unsafe fn from_blob(value: Value) -> *mut Self {
        value.as_blob_mut() as *mut Self
    }

    unsafe fn size(self: *const Self) -> usize {
        (*self).size
    }

    unsafe fn get(self: *mut Self, index: usize) -> *mut NameRecord {
        assert!(index <= self.size());
        let base = self.add(1) as *mut NameRecord; // Skip `NameTable` header
        let record = base.add(index);
        debug_assert!((record as usize) < self.text_section_start() as usize);
        record
    }

    unsafe fn lookup(self: *mut Self, hash: NameHash) -> String {
        match self.index_of(hash) {
            None => undefined_name(hash),
            Some(index) => {
                let record = self.get(index);
                self.text_section_read((*record).offset, (*record).length)
            }
        }
    }

    unsafe fn text_section_start(self: *const Self) -> *const u8 {
        let base = self.add(1) as *mut NameRecord; // Skip `NameTable` header
        base.add(self.size()) as usize as *const u8
    }

    unsafe fn text_section_length(self: *const Self) -> usize {
        let length = block_size(self as usize).to_bytes().as_usize();
        let end = self as usize + length;
        end - self.text_section_start() as usize
    }

    unsafe fn text_section_read(self: *mut Self, offset: usize, length: usize) -> String {
        debug_assert!(offset + length <= self.text_section_length());
        let section = self.text_section_start();
        let slice = from_raw_parts(section.add(offset), length);
        from_utf8(slice).unwrap().to_string()
    }
}

impl SortedArray<NameHash> for *mut NameTable {
    fn get_length(&self) -> usize {
        unsafe { self.size() }
    }

    fn value_at(&self, index: usize) -> NameHash {
        unsafe {
            let record = self.get(index);
            (*record).hash
        }
    }
}

fn undefined_name(hash: NameHash) -> String {
    let buffer = format!(100, "<{hash}>");
    from_utf8(&buffer).unwrap().to_string()
}

pub unsafe fn lookup_name(hash: NameHash) -> String {
    let value = load_name_table();
    if value.is_non_null_ptr() {
        let table = NameTable::from_blob(value);
        table.lookup(hash)
    } else {
        undefined_name(hash)
    }
}
