//! Implements Motoko runtime system

#![no_std]
#![feature(
    arbitrary_self_types,
    panic_info_message,
    assoc_char_funcs,
    core_intrinsics
)]

#[macro_use]
mod print;

#[cfg(feature = "gc")]
mod gc;

#[cfg(feature = "gc")]
mod mark_compact;

mod alloc;
pub mod bigint;
pub mod bitmap;
mod blob_iter;
pub mod buf;
mod char;
pub mod closure_table;
pub mod debug;
mod float;
mod idl;
pub mod leb128;
pub mod mark_stack;
mod mem;
pub mod principal_id;
pub mod text;
pub mod text_iter;
#[allow(non_camel_case_types)]
mod tommath_bindings;
pub mod types;
pub mod utf8;

use types::{Bytes, SkewedPtr};

#[no_mangle]
unsafe extern "C" fn version() -> SkewedPtr {
    text::text_of_str("0.1")
}

extern "C" {
    fn rts_trap(msg: *const u8, len: Bytes<u32>) -> !;

}

pub(crate) unsafe fn trap_with_prefix(prefix: &str, msg: &str) -> ! {
    // Rust currently doesn't support stack-allocated dynamically-sized arrays or alloca, so we
    // have a max bound to the message size here.
    //
    // Messages longer than this are cut off.
    //
    // Relevant RFC: https://github.com/rust-lang/rust/issues/48055
    const BUF_LEN: usize = 512;

    let mut c_str = [0u8; BUF_LEN];
    let mut b_idx = 0;

    for b in prefix.as_bytes() {
        if b_idx == BUF_LEN {
            break;
        }
        c_str[b_idx] = *b;
        b_idx += 1;
    }

    for b in msg.as_bytes() {
        if b_idx == BUF_LEN {
            break;
        }
        c_str[b_idx] = *b;
        b_idx += 1;
    }

    rts_trap(c_str.as_ptr(), Bytes(b_idx as u32));
}

pub(crate) unsafe fn rts_trap_with(msg: &str) -> ! {
    trap_with_prefix("RTS error: ", msg)
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
        rts_trap_with("RTS panicked");
    }
}
