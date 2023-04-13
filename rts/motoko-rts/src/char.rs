use motoko_rts_macros::export;

#[export(ic_only)]
unsafe fn char_to_upper(c: u32) -> u32 {
    let mut upper_chars = core::char::from_u32_unchecked(c).to_uppercase();
    if upper_chars.len() == 1 {
        upper_chars.next().unwrap() as u32
    } else {
        c
    }
}

#[export(ic_only)]
unsafe fn char_to_lower(c: u32) -> u32 {
    let mut lower_chars = core::char::from_u32_unchecked(c).to_lowercase();
    if lower_chars.len() == 1 {
        lower_chars.next().unwrap() as u32
    } else {
        c
    }
}

#[export(ic_only)]
unsafe fn char_is_whitespace(c: u32) -> u32 {
    core::char::from_u32_unchecked(c).is_whitespace().into()
}

#[export(ic_only)]
unsafe fn char_is_uppercase(c: u32) -> u32 {
    core::char::from_u32_unchecked(c).is_uppercase().into()
}

#[export(ic_only)]
unsafe fn char_is_lowercase(c: u32) -> u32 {
    core::char::from_u32_unchecked(c).is_lowercase().into()
}

#[export(ic_only)]
unsafe fn char_is_alphabetic(c: u32) -> u32 {
    core::char::from_u32_unchecked(c).is_alphabetic().into()
}
