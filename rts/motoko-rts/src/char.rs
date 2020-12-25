#[no_mangle]
unsafe extern "C" fn char_to_upper(c: u32) -> u32 {
    let mut upper_chars = core::char::from_u32_unchecked(c).to_uppercase();
    if upper_chars.len() == 1 {
        upper_chars.next().unwrap() as u32
    } else {
        c
    }
}

#[no_mangle]
unsafe extern "C" fn char_to_lower(c: u32) -> u32 {
    let mut lower_chars = core::char::from_u32_unchecked(c).to_lowercase();
    if lower_chars.len() == 1 {
        lower_chars.next().unwrap() as u32
    } else {
        c
    }
}

fn bool_to_u32(b: bool) -> u32 {
    if b {
        1
    } else {
        0
    }
}

#[no_mangle]
unsafe extern "C" fn char_is_whitespace(c: u32) -> u32 {
    bool_to_u32(core::char::from_u32_unchecked(c).is_whitespace())
}

#[no_mangle]
unsafe extern "C" fn char_is_uppercase(c: u32) -> u32 {
    bool_to_u32(core::char::from_u32_unchecked(c).is_uppercase())
}

#[no_mangle]
unsafe extern "C" fn char_is_lowercase(c: u32) -> u32 {
    bool_to_u32(core::char::from_u32_unchecked(c).is_lowercase())
}

#[no_mangle]
unsafe extern "C" fn char_is_alphabetic(c: u32) -> u32 {
    bool_to_u32(core::char::from_u32_unchecked(c).is_alphabetic())
}
