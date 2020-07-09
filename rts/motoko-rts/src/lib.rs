//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]

#[macro_use]
mod print;

mod common;

#[cfg(target_arch = "wasm32")]
mod types;

#[cfg(target_arch = "wasm32")]
mod debug;

#[cfg(target_arch = "wasm32")]
mod alloc;

#[cfg(target_arch = "wasm32")]
mod gc;

#[cfg(target_arch = "wasm32")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { ::core::arch::wasm32::unreachable() }
}

// TODO: Native version should print and abort the process with return code 1.
#[cfg(not(target_arch = "wasm32"))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
