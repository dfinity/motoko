use crate::memory::{TestMemory, set_memory};

use motoko_rts::principal_id::{blob_of_principal, principal_of_blob};
use motoko_rts::text::{text_compare, text_of_ptr_size, text_of_str};
use motoko_rts::types::{Bytes, Words};

pub unsafe fn test() {
    println!("Testing principal id encoding ...");

    set_memory(TestMemory::new(Words(1024 * 1024)));

    //
    // Encoding
    //

    let text = text_of_str("");
    assert_eq!(
        text_compare(
            principal_of_blob(text),
            text_of_str("aaaaa-aa"),
        ),
        0,
    );

    let text = text_of_ptr_size(b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5));
    let principal = principal_of_blob(text);

    assert_eq!(
        text_compare(principal, text_of_str("bfozs-kwa73-7nadi"),),
        0
    );

    //
    // Decoding
    //

    let text = text_of_str("aaaaa-aa");
    let principal = blob_of_principal(text);
    assert_eq!(text_compare(principal, text_of_str(""),), 0);

    let text = text_of_str("bfozs-kwa73-7nadi");
    assert_eq!(
        text_compare(
            blob_of_principal(text),
            text_of_ptr_size(b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))
        ),
        0
    );
}
