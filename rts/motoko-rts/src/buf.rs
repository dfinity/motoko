//! This module implements a simple buffer to be used by the compiler (in generated code)

use crate::idl_trap_with;

#[repr(packed)]
pub struct Buf {
    /// Pointer into the buffer
    pub ptr: *mut u8,
    /// Pointer to the end of the buffer
    pub end: *mut u8,
}

impl Buf {
    #[cfg(feature = "ic")]
    pub(crate) unsafe fn advance(self: *mut Self, n: u32) {
        advance(self, n)
    }
}

/// Read a single byte
pub(crate) unsafe fn read_byte(buf: *mut Buf) -> u8 {
    if (*buf).ptr >= (*buf).end {
        idl_trap_with("byte read out of buffer");
    }

    let byte = *(*buf).ptr;
    (*buf).ptr = (*buf).ptr.add(1);

    byte
}

#[cfg(feature = "ic")]
/// Read a little-endian word
pub(crate) unsafe fn read_word(buf: *mut Buf) -> u32 {
    if (*buf).ptr.add(3) >= (*buf).end {
        idl_trap_with("word read out of buffer");
    }

    let p = (*buf).ptr;
    let word = u32::from_le_bytes([*p, *p.add(1), *p.add(2), *p.add(3)]);

    (*buf).ptr = (*buf).ptr.add(4);

    word
}

#[cfg(feature = "ic")]
unsafe fn advance(buf: *mut Buf, n: u32) {
    if (*buf).ptr.add(n as usize) > (*buf).end {
        idl_trap_with("advance out of buffer");
    }

    (*buf).ptr = (*buf).ptr.add(n as usize);
}

/// Can also be used for sleb
#[cfg(feature = "ic")]
#[no_mangle]
pub(crate) unsafe extern "C" fn skip_leb128(buf: *mut Buf) {
    loop {
        let byte = read_byte(buf);
        if byte & 0b1000_0000 == 0 {
            break;
        }
    }
}
