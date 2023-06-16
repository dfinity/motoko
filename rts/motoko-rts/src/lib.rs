//! Implements Motoko runtime system

#![no_std]
#![feature(
    arbitrary_self_types,
    core_intrinsics,
    panic_info_message,
    proc_macro_hygiene,
    // We do not need simd but this flag enables `core::arch:wasm64`.
    // See https://github.com/rust-lang/rust/issues/90599
    simd_wasm64
)]


#[macro_use]
pub mod print;

// #[cfg(debug_assertions)]
// pub mod debug;

// mod barriers;
// pub mod bigint;
// pub mod bitrel;
// #[cfg(feature = "ic")]
// mod blob_iter;
// pub mod buf;
// mod char;
// pub mod constants;
// pub mod continuation_table;
// #[cfg(feature = "ic")]
// mod float;
// pub mod gc;
// #[cfg(feature = "ic")]
// mod idl;
// pub mod leb128;
// mod mem_utils;
// pub mod memory;
// pub mod principal_id;
// mod static_checks;
// pub mod stream;
// pub mod text;
// pub mod text_iter;
// mod tommath_bindings;
// pub mod types;
// pub mod utf8;
// mod visitor;

// use types::Bytes;

use core::intrinsics::size_of;

use motoko_rts_macros::*;

// #[ic_mem_fn(ic_only)]
// unsafe fn version<M: memory::Memory>(mem: &mut M) -> types::Value {
//     text::text_of_str(mem, "0.1")
// }

// #[non_incremental_gc]
// #[ic_mem_fn(ic_only)]
// unsafe fn alloc_words<M: memory::Memory>(mem: &mut M, n: types::Words<u32>) -> types::Value {
//     mem.alloc_words(n)
// }

// #[incremental_gc]
// #[ic_mem_fn(ic_only)]
// unsafe fn alloc_words<M: memory::Memory>(mem: &mut M, n: types::Words<u32>) -> types::Value {
//     crate::gc::incremental::get_partitioned_heap().allocate(mem, n)
// }

// extern "C" {
//     fn rts_trap(msg: *const u8, len: Bytes<u32>) -> !;
// }

// pub(crate) unsafe fn trap_with_prefix(prefix: &str, msg: &str) -> ! {
//     // Rust currently doesn't support stack-allocated dynamically-sized arrays or alloca, so we
//     // have a max bound to the message size here.
//     //
//     // Messages longer than this are cut off.
//     //
//     // Relevant RFC: https://github.com/rust-lang/rust/issues/48055
//     const BUF_LEN: usize = 512;

//     let mut c_str = [0u8; BUF_LEN];
//     let mut b_idx = 0;

//     for b in prefix.as_bytes() {
//         if b_idx == BUF_LEN {
//             break;
//         }
//         c_str[b_idx] = *b;
//         b_idx += 1;
//     }

//     for b in msg.as_bytes() {
//         if b_idx == BUF_LEN {
//             break;
//         }
//         c_str[b_idx] = *b;
//         b_idx += 1;
//     }

//     rts_trap(c_str.as_ptr(), Bytes(b_idx as u32));
// }

// pub(crate) unsafe fn idl_trap_with(msg: &str) -> ! {
//     trap_with_prefix("IDL error: ", msg);
// }

// pub(crate) unsafe fn rts_trap_with(msg: &str) -> ! {
//     trap_with_prefix("RTS error: ", msg)
// }

// TODO: Remove temporary code used during 64-bit porting
const fn skew(ptr: u64) -> u64 {
    ptr.wrapping_sub(1)
}

// TODO: Remove temporary code used during 64-bit porting
const fn unskew(value: u64) -> u64 {
    value.wrapping_add(1)
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn blob_of_text(s: u64) -> u64 {
    let tag = unskew(s) as *mut u32;
    if *tag == 17 {
        s
    } else {
        panic!("Not supported");
    }
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_to_word64_trap(_p: u32) -> u64 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_lt(_left: u32, _right: u32) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_of_int64(_value: u64) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_of_word64(_value: u64) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_to_word32_wrap(_p: u32) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_2complement_bits(_p: u32) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn bigint_add(_left: u32, right: u32) -> u32 {
    panic!("Not supported");
}

// TODO: Remove temporary code used during 64-bit porting
static mut HP: u64 = 0;

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn test_initialize() {
    HP = get_heap_base();
}

extern "C" {
    fn get_heap_base() -> u64;
}

// TODO: Remove temporary code used during 64-bit porting
#[no_mangle]
pub unsafe extern "C" fn alloc_array(length: u64) -> u64 {
    use core::arch::wasm64;
    const HEADER_SIZE: u64 = 2;
    let bytes = (length + HEADER_SIZE) * size_of::<u64>() as u64;
    let old_hp = HP;
    HP += bytes;
    if HP > (wasm64::memory_size(0) << 16) as u64 {
        grow_memory(HP)
    }
    println!(100, "ALLOCATED {old_hp} {length} {bytes}");
    skew(old_hp)
}

unsafe fn grow_memory(ptr: u64) {
    use core::arch::wasm64;
    const WASM_PAGE_SIZE: u64 = 64 * 1024;
    debug_assert_eq!(0xFFFF_FFFF_FFFF_0000, u64::MAX - WASM_PAGE_SIZE + 1);
    if ptr > 0xFFFF_0000 {
        // spare the last wasm memory page
        panic!("Cannot grow memory")
    };
    let page_size = u64::from(WASM_PAGE_SIZE);
    let total_pages_needed = (ptr + page_size - 1) / page_size;
    let current_pages = wasm64::memory_size(0) as u64;
    if total_pages_needed > current_pages {
        if wasm64::memory_grow(0, (total_pages_needed - current_pages) as usize) == usize::MAX {
            // replica signals that there is not enough memory
            panic!("Cannot grow memory");
        }
    }
}

#[cfg(feature = "ic")]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    unsafe { println!(100, "PANIC"); }
    loop {}
    // unsafe {
    //     if let Some(msg) = info.payload().downcast_ref::<&str>() {
    //         println!(1000, "RTS panic: {}", msg);
    //     } else if let Some(args) = info.message() {
    //         let mut buf = [0 as u8; 1000];
    //         let mut fmt = print::WriteBuf::new(&mut buf);
    //         let _ = core::fmt::write(&mut fmt, *args);
    //         print::print(&fmt);
    //     } else {
    //         println!(1000, "RTS panic: weird payload");
    //     }

    //     if let Some(location) = info.location() {
    //         println!(
    //             1000,
    //             "panic occurred in file '{}' at line {}",
    //             location.file(),
    //             location.line(),
    //         );
    //     }

    //     rts_trap_with("RTS panicked");
    // }
}
