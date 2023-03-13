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


    // Big endian.
    pub fn read_u16(offset: u64) -> u16 {
	let mut res : [u8; 2] = [0, 0];
	read(offset, &mut res);
	(res[0] as u16) << 8 | res[1] as u16 // big endian u16
    }

    // Big endian.
    pub fn write_u16(offset: u64, value: u16) {
	let n_ = value as u16;
	let bytes : [u8; 2] = [((n_ & 0xFF00) >> 8) as u8, (n_ & 0xFF) as u8];
	write(offset, &bytes);
    }

    // Big endian.
    pub fn read_u64(offset: u64) -> u64 {
	let mut res : [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
	read(offset, &mut res);
	(res[0] as u64) << (8 * 7) |
	(res[1] as u64) << (8 * 6) |
	(res[2] as u64) << (8 * 5) |
	(res[3] as u64) << (8 * 4) |
	(res[4] as u64) << (8 * 3) |
	(res[5] as u64) << (8 * 2) |
	(res[6] as u64) << (8 * 1) |
	(res[7] as u64) << (8 * 0)
    }

    // Big endian.
    pub fn write_u64(offset: u64, n: u64) {
	let bytes : [u8; 8] = [
	    ((n & 0xFF00000000000000) >> (8 * 7)) as u8,
	    ((n & 0x00FF000000000000) >> (8 * 6)) as u8,
	    ((n & 0x0000FF0000000000) >> (8 * 5)) as u8,
	    ((n & 0x000000FF00000000) >> (8 * 4)) as u8,
	    ((n & 0x00000000FF000000) >> (8 * 3)) as u8,
	    ((n & 0x0000000000FF0000) >> (8 * 2)) as u8,
	    ((n & 0x000000000000FF00) >> (8 * 1)) as u8,
	    ((n & 0x00000000000000FF) >> (8 * 0)) as u8,
	];
	write(offset, &bytes);
    }

}
