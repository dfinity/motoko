extern "C" {
    pub fn rts_trap_with(msg: *const u8) -> !;
}

/*
#[cfg(target_arch = "wasm32")]
mod ic0 {
    // NB: The #[link(...)] line below really needs to be before `extern "C"` part, we can't move
    // it inside the extern block, it doesn't work.
    #[link(wasm_import_module = "ic0")]
    extern "C" {
        #[no_mangle]
        pub fn debug_print(msg: *const u8, len: u32);
    }
}
*/

#[cfg(target_arch = "wasm32")]
pub unsafe fn debug_print(s: &str) {
    // ic0::debug_print(s.as_ptr(), s.len() as u32)
    libc::printf("%s\n".as_ptr() as *const _, s);
}
