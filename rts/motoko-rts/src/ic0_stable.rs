#[link(wasm_import_module = "ic0")]
extern "C" {
    pub fn stable64_size() -> u64;
    pub fn stable64_grow(additional_pages: u64) -> i64;
    pub fn stable64_read(dst: u64, offset: u64, size: u64);
    pub fn stable64_write(offset: u64, src: u64, size: u64);
}

// to do -- rename this module something better.
pub mod nicer {
    use crate::ic0_stable::*;

    pub fn size() -> u64 {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_size() }
    }

    pub fn grow(pages: u64) -> i64 {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_grow(pages) }
    }

    pub fn read(offset: u64, dst: &mut [u8]) {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_read(dst.as_ptr() as u64, offset, dst.len() as u64) }
    }

    pub fn write(offset: u64, src: &[u8]) {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_write(offset, src.as_ptr() as u64, src.len() as u64) }
    }
}
