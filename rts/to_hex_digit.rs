#![no_std]

extern "C" {
    fn rts_trap_with(msg: *const u8) -> !;
}

#[no_mangle]
pub unsafe fn to_hex_digit(n: u8) -> u32 {
    if n < 10 {
        u32::from(b'0' + n)
    } else if n < 16 {
        u32::from(b'A' + (n - 10))
    } else {
        rts_trap_with("to_hex_digit: out_of_range\0".as_ptr());
    }
}
