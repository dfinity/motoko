//! Implements Motoko runtime system

#![no_std]
// TODO (osa): Some of these are stabilized, we need to update rustc
#![feature(
    arbitrary_self_types,
    panic_info_message,
    assoc_char_funcs,
    core_intrinsics,
    ptr_offset_from
)]

#[macro_use]
mod print;

#[cfg(debug_assertions)]
pub mod debug;

#[cfg(feature = "ic")]
mod allocation_area;
pub mod bigint;
#[cfg(feature = "ic")]
mod blob_iter;
pub mod buf;
mod char;
pub mod constants;
pub mod continuation_table;
#[cfg(feature = "ic")]
mod float;
//pub mod gc;
#[cfg(feature = "ic")]
mod idl;
pub mod leb128;
mod mem_utils;
pub mod memory;
#[cfg(feature = "ic")]
mod page_alloc;
pub mod principal_id;
mod space;
pub mod text;
pub mod text_iter;
mod tommath_bindings;
pub mod types;
pub mod utf8;
mod visitor;

use types::Bytes;

use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn(ic_only)]
unsafe fn version<M: memory::Memory>(mem: &mut M) -> types::SkewedPtr {
    text::text_of_str(mem, "0.1")
}

extern "C" {
    fn rts_trap(msg: *const u8, len: Bytes<u32>) -> !;
}

/// Initialize the runtime system
// #[cfg(feature = "ic")]
// #[no_mangle]
// unsafe extern "C" fn init() {
//     allocation_area::init();
// }

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

pub(crate) unsafe fn idl_trap_with(msg: &str) -> ! {
    trap_with_prefix("IDL error: ", msg);
}

pub(crate) unsafe fn rts_trap_with(msg: &str) -> ! {
    trap_with_prefix("RTS error: ", msg)
}

#[cfg(feature = "ic")]
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

        if let Some(location) = info.location() {
            println!(
                1000,
                "panic occurred in file '{}' at line {}",
                location.file(),
                location.line(),
            );
        }

        rts_trap_with("RTS panicked");
    }
}
