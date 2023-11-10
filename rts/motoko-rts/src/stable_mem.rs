pub const PAGE_SIZE: u64 = 64 * 1024;

extern "C" {
    // physical ic0_stable64 operations re-exported by moc
    pub fn ic0_stable64_write(offset: u64, src: u64, size: u64);
    pub fn ic0_stable64_read(dst: u64, offset: u64, size: u64);
    // (virtual) stable_mem operations implemented by moc
    pub fn moc_stable_mem_get_version() -> u32;
    pub fn moc_stable_mem_set_version(version: u32);
    pub fn moc_stable_mem_size() -> u64;
    pub fn moc_stable_mem_grow(additional_pages: u64) -> u64;
}

pub fn get_version() -> u32 {
    unsafe { moc_stable_mem_get_version() }
}

pub fn set_version(version: u32) {
    unsafe { moc_stable_mem_set_version(version) }
}

pub fn size() -> u64 {
    // SAFETY: This is safe because of the ic0 api guarantees.
    unsafe { moc_stable_mem_size() }
}

pub fn grow(pages: u64) -> u64 {
    // SAFETY: This is safe because of the ic0 api guarantees.
    unsafe { moc_stable_mem_grow(pages) }
}

pub fn read(offset: u64, dst: &mut [u8]) {
    // SAFETY: This is safe because of the ic0 api guarantees.
    unsafe { ic0_stable64_read(dst.as_ptr() as u64, offset, dst.len() as u64) }
}

pub fn write(offset: u64, src: &[u8]) {
    // SAFETY: This is safe because of the ic0 api guarantees.
    unsafe { ic0_stable64_write(offset, src.as_ptr() as u64, src.len() as u64) }
}

// Little endian.
pub fn read_u8(offset: u64) -> u8 {
    let mut res: [u8; 1] = [0; 1];
    read(offset, &mut res);
    core::primitive::u8::from_le_bytes(res)
}

// Little endian.
pub fn write_u8(offset: u64, value: u8) {
    write(offset, &core::primitive::u8::to_le_bytes(value));
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
