//! This module implements a simple buffer to be used by the compiler (in generated code)

#[repr(C)]
pub(crate) struct Buf {
    /// Pointer into the buffer
    pub(crate) ptr: *mut u8,
    /// Pointer to the end of the buffer
    pub(crate) end: *mut u8,
}

impl Buf {
    pub(crate) unsafe fn advance(self: *mut Self, n: u32) {
        advance(self, n)
    }
}

extern "C" {
    // TODO: Why not rts_trap_with?
    fn idl_trap_with(msg: *const u8) -> !;
}

/// Read a single byte
#[no_mangle]
pub(crate) unsafe extern "C" fn read_byte(buf: *mut Buf) -> u8 {
    if (*buf).ptr >= (*buf).end {
        idl_trap_with("byte read out of buffer\0".as_ptr());
    }

    let byte = *(*buf).ptr;
    (*buf).ptr = (*buf).ptr.add(1);

    byte
}

/// Read a little-endian word
#[no_mangle]
pub(crate) unsafe extern "C" fn read_word(buf: *mut Buf) -> u32 {
    if (*buf).ptr.add(3) >= (*buf).end {
        idl_trap_with("word read out of buffer\0".as_ptr());
    }

    let p = (*buf).ptr;
    let word = u32::from_le_bytes([*p, *p.add(1), *p.add(2), *p.add(3)]);

    (*buf).ptr = (*buf).ptr.add(4);

    word
}

#[no_mangle]
unsafe extern "C" fn advance(buf: *mut Buf, n: u32) {
    if (*buf).ptr.add(n as usize) > (*buf).end {
        idl_trap_with("advance out of buffer\0".as_ptr());
    }

    (*buf).ptr = (*buf).ptr.add(n as usize);
}

/// Can also be used for sleb
#[no_mangle]
unsafe extern "C" fn skip_leb128(buf: *mut Buf) {
    loop {
        let byte = read_byte(buf);
        if byte & 0b1000_0000 == 0 {
            break;
        }
    }
}
