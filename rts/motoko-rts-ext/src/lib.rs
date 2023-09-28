//! Extends Motoko runtime system (experiment)

#![no_std]
#![feature(
    arbitrary_self_types,
    core_intrinsics,
    panic_info_message,
    proc_macro_hygiene
)]

// use motoko_rts::types::Value;

/// A value in a heap slot
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Value(u32);

// Return the argument as the result.

#[cfg(feature = "ic")]
pub unsafe fn test_identity(v: Value) -> Value {
    v
}


#[cfg(feature = "ic")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    todo!()
}
