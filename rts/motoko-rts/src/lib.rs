//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]

extern "C" {
    pub(crate) fn rts_trap_with(msg: *const u8) -> !;
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe {
        rts_trap_with("RTS panicked\0".as_ptr());
    }
}
