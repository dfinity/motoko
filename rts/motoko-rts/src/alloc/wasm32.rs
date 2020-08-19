use crate::gc;
use crate::types::*;

#[no_mangle]
pub unsafe extern "C" fn alloc_bytes(n: Bytes<u32>) -> SkewedPtr {
    alloc_words(bytes_to_words(n))
}

#[no_mangle]
pub unsafe extern "C" fn alloc_words(n: Words<u32>) -> SkewedPtr {
    let bytes = words_to_bytes(n);
    // Update ALLOCATED
    gc::ALLOCATED.0 += bytes.0 as u64;

    // Update heap pointer
    let old_hp = gc::get_hp();
    let new_hp = old_hp + bytes.0 as usize;
    gc::set_hp(new_hp);

    // Grow memory if needed
    gc::grow_memory(new_hp);

    skew(old_hp)
}
