#[cfg(target_arch = "wasm32")]
mod wasm32;

#[cfg(target_arch = "wasm32")]
pub use wasm32::*;

#[cfg(not(target_arch = "wasm32"))]
mod native;

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;
