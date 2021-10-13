use crate::page_alloc::TestPageAlloc;

use motoko_rts::principal_id::{blob_of_principal, principal_of_blob};
use motoko_rts::space::Space;
use motoko_rts::text::{text_compare, text_of_ptr_size, text_of_str};
use motoko_rts::types::Bytes;

pub unsafe fn test() {
    println!("Testing principal id encoding ...");

    let page_alloc = TestPageAlloc::new();
    let mut space = Space::new(page_alloc);

    //
    // Encoding
    //

    let text = text_of_str(&mut space, "");
    assert_eq!(
        text_compare(
            principal_of_blob(&mut space, text),
            text_of_str(&mut space, "aaaaa-aa"),
        ),
        0,
    );

    let text = text_of_ptr_size(&mut space, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5));
    let principal = principal_of_blob(&mut space, text);

    assert_eq!(
        text_compare(principal, text_of_str(&mut space, "bfozs-kwa73-7nadi"),),
        0
    );

    //
    // Decoding
    //

    let text = text_of_str(&mut space, "aaaaa-aa");
    let principal = blob_of_principal(&mut space, text);
    assert_eq!(text_compare(principal, text_of_str(&mut space, ""),), 0);

    let text = text_of_str(&mut space, "bfozs-kwa73-7nadi");
    assert_eq!(
        text_compare(
            blob_of_principal(&mut space, text),
            text_of_ptr_size(&mut space, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))
        ),
        0
    );
}
