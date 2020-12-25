//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]
#![feature(arbitrary_self_types, panic_info_message, assoc_char_funcs)]

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
mod text_iter;
pub mod types;

extern "C" {
    pub(crate) fn rts_trap_with(msg: *const u8) -> !;
}

#[cfg(feature = "panic_handler")]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    unsafe {
        if let Some(msg) = info.payload().downcast_ref::<&str>() {
            println!(1000, "RTS panic: {}", msg);
        } else if let Some(args) = info.message() {
            let mut buf = [0 as u8; 1000];
            let mut fmt = print::WriteBuf::new(&mut buf);
            let _ = core::fmt::write(&mut fmt, *args);
            print::print(&fmt);
        } else {
            println!(1000, "RTS panic: weird payload");
        }
        rts_trap_with("RTS panicked\0".as_ptr());
    }
}
