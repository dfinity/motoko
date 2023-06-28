use crate::memory::Memory;
use crate::text::text_of_ptr_size;
use crate::types::{Bytes, Value};

use motoko_rts_macros::ic_mem_fn;

// The meaning of the `mode` parameter is documented in motoko-base, function Float.format()
#[ic_mem_fn]
unsafe fn float_fmt<M: Memory>(mem: &mut M, a: f64, prec: u32, mode: u32) -> Value {
    // prec and mode are tagged small words (`Nat8`s), so we shift 24 bits. See
    // `TaggedSmallWord.bits_of_type` in compile.ml.
    let mode = mode >> 24;
    let prec = core::cmp::min(prec >> 24, 100) as usize;

    // 320 bytes needed for max precision (1.7e308)
    const BUFFER_LENGTH: usize = 320;
    let buffer = match mode {
        0 => format!(BUFFER_LENGTH, "{:.*}", prec, a),
        1 => format!(BUFFER_LENGTH, "{:.*e}", prec, a),
        2 => format!(BUFFER_LENGTH, "{:.*}", prec, a),
        3 => panic!("float_fmt: unsupported mode"), // TODO: Support this mode (or deprecate in base library).
        4 => format!(BUFFER_LENGTH, "{}", a),
        _ => panic!("float_fmt: unrecognized mode"),
    };

    // TODO: Certain modes are not supported such as hexadecimal output (mode 3).

    text_of_ptr_size(mem, buffer.as_ptr(), Bytes(BUFFER_LENGTH))
}
