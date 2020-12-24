//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]
#![feature(arbitrary_self_types)]

#[macro_use]
mod print;

#[cfg(feature = "gc")]
mod gc;

mod alloc;
mod buf;
pub mod closure_table;
mod debug;
mod mem;
mod text;
pub mod types;

extern "C" {
    pub(crate) fn rts_trap_with(msg: *const u8) -> !;
}

#[cfg(feature = "panic_handler")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe {
        rts_trap_with("RTS panicked\0".as_ptr());
    }
}
