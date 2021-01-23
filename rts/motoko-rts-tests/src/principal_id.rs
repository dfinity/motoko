use motoko_rts::principal_id::{blob_of_principal, principal_of_blob};
use motoko_rts::text::{text_compare, text_of_ptr_size, text_of_str};
use motoko_rts::types::Bytes;

pub unsafe fn test() {
    println!("Testing principal id encoding ...");

    //
    // Encoding
    //

    assert_eq!(
        text_compare(principal_of_blob(text_of_str("")), text_of_str("aaaaa-aa"),),
        0,
    );

    assert_eq!(
        text_compare(
            principal_of_blob(text_of_ptr_size(b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))),
            text_of_str("bfozs-kwa73-7nadi"),
        ),
        0
    );

    //
    // Decoding
    //

    assert_eq!(
        text_compare(blob_of_principal(text_of_str("aaaaa-aa")), text_of_str(""),),
        0
    );

    assert_eq!(
        text_compare(
            blob_of_principal(text_of_str("bfozs-kwa73-7nadi")),
            text_of_ptr_size(b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))
        ),
        0
    );
}
