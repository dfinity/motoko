//! This module implements a simple buffer to be used by the compiler (in generated code)

use crate::idl_trap_with;

#[repr(packed)]
pub struct Buf {
    /// Pointer into the buffer
    pub ptr: *mut u8,
    /// Pointer to the end of the buffer
    pub end: *mut u8,
    pub decoding_quota: usize,
    pub skipping_quota: usize,
}

impl Buf {
    #[cfg(feature = "ic")]
    pub(crate) unsafe fn advance(self: *mut Self, n: u32) {
        advance(self, n)
    }

    #[cfg(feature = "ic")]
    pub(crate) unsafe fn add_cost(self: *mut Self, skip: bool, cost: usize) {
        if (*self).decoding_quota > 0 {
            let cost = if skip { cost * 50 } else { cost };
            if (*self).decoding_quota < cost {
                idl_trap_with("decoding cost exceeds the limit");
            }
            (*self).decoding_quota = (*self).decoding_quota - cost;
        }
        if skip {
            if (*self).skipping_quota > 0 {
                if (*self).skipping_quota < cost {
                    idl_trap_with("skipping cost exceeds the limit")
                }
                (*self).skipping_quota = (*self).skipping_quota - cost;
            }
        }
    }

    #[cfg(feature = "ic")]
    pub(crate) unsafe fn add_decode_cost(self: *mut Self, cost: usize) {
        self.add_cost(false, cost);
    }

    #[cfg(feature = "ic")]
    pub(crate) unsafe fn add_skip_cost(self: *mut Self, cost: usize) {
        self.add_cost(true, cost);
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
        buf.add_skip_cost(1); // TBR
        let byte = read_byte(buf);
        if byte & 0b1000_0000 == 0 {
            break;
        }
    }
}

#[cfg(feature = "ic")]
#[no_mangle]
pub(crate) unsafe extern "C" fn idl_decode_cost(buf: *mut Buf, cost: usize) {
    buf.add_decode_cost(cost);
}

#[cfg(feature = "ic")]
#[no_mangle]
pub(crate) unsafe extern "C" fn idl_skip_cost(buf: *mut Buf, cost: usize) {
    buf.add_skip_cost(cost);
}

#[cfg(feature = "ic")]
#[no_mangle]
pub(crate) unsafe extern "C" fn idl_set_quotas(
    buf: *mut Buf,
    decoding_quota: usize,
    skipping_quota: usize,
) {
    (*buf).decoding_quota = decoding_quota;
    (*buf).skipping_quota = skipping_quota;
}
