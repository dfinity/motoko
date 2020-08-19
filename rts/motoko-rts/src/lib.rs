//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]

mod alloc;
mod array;
mod closure_table;
mod common;
mod types;

#[cfg(target_arch = "wasm32")]
mod gc;

#[cfg(target_arch = "wasm32")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { ::core::arch::wasm32::unreachable() }
}

#[cfg(not(target_arch = "wasm32"))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
