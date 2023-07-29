extern "C" {
    pub fn stable64_write_moc(offset: u64, src: u64, size: u64);
    pub fn stable64_read_moc(dst: u64, offset: u64, size: u64);
    pub fn stable64_size_moc() -> u64;
    pub fn stable64_grow_moc(additional_pages: u64) -> u64;
}

// to do -- rename this module something better.
pub mod nicer {
    use crate::ic0_stable::*;

    pub fn size() -> u64 {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_size_moc() }
    }

    pub fn grow(pages: u64) -> u64 {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_grow_moc(pages) }
    }

    pub fn read(offset: u64, dst: &mut [u8]) {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_read_moc(dst.as_ptr() as u64, offset, dst.len() as u64) }
    }

    pub fn write(offset: u64, src: &[u8]) {
        // SAFETY: This is safe because of the ic0 api guarantees.
        unsafe { stable64_write_moc(offset, src.as_ptr() as u64, src.len() as u64) }
    }

    // Little endian.
    pub fn read_u16(offset: u64) -> u16 {
        let mut res: [u8; 2] = [0; 2];
        read(offset, &mut res);
        core::primitive::u16::from_le_bytes(res)
    }

    // Little endian.
    pub fn write_u16(offset: u64, value: u16) {
        write(offset, &core::primitive::u16::to_le_bytes(value));
    }

    // Little endian.
    pub fn read_u32(offset: u64) -> u32 {
        let mut res: [u8; 4] = [0; 4];
        read(offset, &mut res);
        core::primitive::u32::from_le_bytes(res)
    }

    // Little endian.
    pub fn write_u32(offset: u64, value: u32) {
        write(offset, &core::primitive::u32::to_le_bytes(value));
    }

    // Little endian.
    pub fn read_u64(offset: u64) -> u64 {
        let mut res: [u8; 8] = [0; 8];
        read(offset, &mut res);
        core::primitive::u64::from_le_bytes(res)
    }

    // Little endian.
    pub fn write_u64(offset: u64, n: u64) {
        write(offset, &core::primitive::u64::to_le_bytes(n));
    }
}
