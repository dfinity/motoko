use crate::memory::{initialize_test_memory, reset_test_memory};

use motoko_rts::principal_id::{blob_of_principal, principal_of_blob};
use motoko_rts::text::{text_compare, text_of_ptr_size, text_of_str};
use motoko_rts::types::Bytes;

pub unsafe fn test() {
    println!("Testing principal id encoding ...");

    let mut heap = initialize_test_memory();

    //
    // Encoding
    //

    let text = text_of_str(&mut heap, "");
    assert_eq!(
        text_compare(
            principal_of_blob(&mut heap, text),
            text_of_str(&mut heap, "aaaaa-aa"),
        ),
        0,
    );

    let text = text_of_ptr_size(&mut heap, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5));
    let principal = principal_of_blob(&mut heap, text);

    assert_eq!(
        text_compare(principal, text_of_str(&mut heap, "bfozs-kwa73-7nadi"),),
        0
    );

    //
    // Decoding
    //

    let text = text_of_str(&mut heap, "aaaaa-aa");
    let principal = blob_of_principal(&mut heap, text);
    assert_eq!(text_compare(principal, text_of_str(&mut heap, ""),), 0);

    let text = text_of_str(&mut heap, "bfozs-kwa73-7nadi");
    assert_eq!(
        text_compare(
            blob_of_principal(&mut heap, text),
            text_of_ptr_size(&mut heap, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))
        ),
        0
    );

    reset_test_memory();
}
