use crate::rts_trap_with;

/// Panics if the string is not valid UTF-8
#[no_mangle]
unsafe extern "C" fn utf8_validate(str: *const libc::c_char, len: u32) {
    if let Err(_) = core::str::from_utf8(core::slice::from_raw_parts(str as *const _, len as usize))
    {
        rts_trap_with("utf8_validate: string is not UTF-8");
    }
}
