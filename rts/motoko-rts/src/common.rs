use core::fmt;

extern "C" {
    pub fn rts_trap_with(msg: *const u8) -> !;
}

#[cfg(target_arch = "wasm32")]
pub mod ic0 {
    // NB: The #[link(...)] line below really needs to be before `extern "C"` part, we can't move
    // it inside the extern block, it doesn't work.
    #[link(wasm_import_module = "ic0")]
    extern "C" {
        #[no_mangle]
        pub fn debug_print(msg: *const u8, len: u32);
    }
}

#[cfg(target_arch = "wasm32")]
pub unsafe fn debug_print(s: &str) {
    ic0::debug_print(s.as_ptr(), s.len() as u32)
    // libc::printf("%s\n".as_ptr() as *const _, s);
}

pub struct FmtWrite<'a> {
    buf: &'a mut [u8],
    offset: usize,
}

impl<'a> FmtWrite<'a> {
    pub fn new(buf: &'a mut [u8]) -> Self {
        Self {
            buf: buf,
            offset: 0,
        }
    }

    pub fn reset(&mut self) {
        self.offset = 0;
    }

    #[cfg(target_arch = "wasm32")]
    pub fn print(&self) {
        unsafe { ic0::debug_print(self.buf.as_ptr(), self.offset as u32) };
    }
}

// https://stackoverflow.com/questions/39488327/how-to-format-output-to-a-byte-array-with-no-std-and-no-allocator
impl<'a> fmt::Write for FmtWrite<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let bytes = s.as_bytes();
        // Skip over already-copied data
        let remainder = &mut self.buf[self.offset..];
        // Check if there is space remaining (return error instead of panicking)
        if remainder.len() < bytes.len() {
            return Err(core::fmt::Error);
        }
        // Make the two slices the same length
        let remainder = &mut remainder[..bytes.len()];
        // Copy
        remainder.copy_from_slice(bytes);
        // Update offset to avoid overwriting
        self.offset += bytes.len();

        Ok(())
    }
}
