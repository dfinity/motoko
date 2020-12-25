//! LEB1128 encoding. Reference: https://en.wikipedia.org/wiki/LEB128

#[no_mangle]
unsafe extern "C" fn leb128_encode(mut val: u32, mut buf: *mut u8) {
    loop {
        let byte = (val & 0b0111_1111) as u8;
        val >>= 7;
        if val == 0 {
            *buf = byte;
            break;
        } else {
            *buf = byte | 0b1000_0000;
            buf = buf.add(1);
        }
    }
}

#[no_mangle]
unsafe extern "C" fn sleb128_encode(mut val: i32, mut buf: *mut u8) {
    loop {
        let byte = (val & 0b0111_1111) as u8;
        val >>= 7;
        if (val == 0 && byte & 0b0100_0000 == 0) || (val == -1 && byte & 0b0100_0000 == 0b0100_0000)
        {
            *buf = byte;
            break;
        } else {
            *buf = byte | 0b1000_0000;
            buf = buf.add(1);
        }
    }
}
