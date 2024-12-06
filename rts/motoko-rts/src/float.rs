use crate::memory::Memory;
use crate::text::text_of_ptr_size;
use crate::types::{Bytes, Value};

use motoko_rts_macros::ic_mem_fn;

// The meaning of the `mode` parameter is documented in motoko-base, function Float.format()
#[ic_mem_fn]
pub unsafe fn float_fmt<M: Memory>(mem: &mut M, a: f64, prec: usize, mode: usize) -> Value {
    // prec and mode are tagged small words (`Nat8`s), so we shift 56 or 24 bits.
    // See `TaggedSmallWord.bits_of_type` in `compile_enhanced.ml` or `compile_classical.ml`.
    const SHIFT: u32 = usize::BITS - 8;
    let mode = mode >> SHIFT;
    let prec = core::cmp::min(prec >> SHIFT, 100) as usize;

    // 320 bytes needed for max precision (1.7e308)
    const BUFFER_LENGTH: usize = 320;
    let buffer = match mode {
        0 => format!(BUFFER_LENGTH, "{:.*}", prec, a),
        1 => format!(BUFFER_LENGTH, "{:.*e}", prec, a),
        2 => format!(BUFFER_LENGTH, "{:.*}", prec, a),
        3 => panic!("float_fmt: unsupported mode"), // Deprecated in the base library.
        4 => format!(BUFFER_LENGTH, "{}", a),
        _ => panic!("float_fmt: unrecognized mode"),
    };

    // TODO: Certain modes are not supported such as hexadecimal output (mode 3).

    let length = written_length(&buffer);
    text_of_ptr_size(mem, buffer.as_ptr(), Bytes(length))
}

fn written_length(buffer: &[u8]) -> usize {
    for index in 0..buffer.len() {
        if buffer[index] == 0 {
            return index;
        }
    }
    buffer.len()
}
