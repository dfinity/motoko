//! Implements bits and pieces of Motoko runtime system. Currently garbage collection and a few
//! utilities.

#![no_std]

pub mod array;
pub mod common;
pub mod types;

#[cfg(target_arch = "wasm32")]
pub mod gc;
