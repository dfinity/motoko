use motoko_rts::heap::Heap;
use motoko_rts::principal_id::{blob_of_principal, principal_of_blob};
use motoko_rts::text::{text_compare, text_of_ptr_size, text_of_str};
use motoko_rts::types::Bytes;

pub unsafe fn test<H: Heap>(heap: &mut H) {
    println!("Testing principal id encoding ...");

    //
    // Encoding
    //

    let text = text_of_str(heap, "");
    assert_eq!(
        text_compare(principal_of_blob(heap, text), text_of_str(heap, "aaaaa-aa"),),
        0,
    );

    let text = text_of_ptr_size(heap, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5));
    let principal = principal_of_blob(heap, text);

    assert_eq!(
        text_compare(principal, text_of_str(heap, "bfozs-kwa73-7nadi"),),
        0
    );

    //
    // Decoding
    //

    let text = text_of_str(heap, "aaaaa-aa");
    let principal = blob_of_principal(heap, text);
    assert_eq!(text_compare(principal, text_of_str(heap, ""),), 0);

    let text = text_of_str(heap, "bfozs-kwa73-7nadi");
    assert_eq!(
        text_compare(
            blob_of_principal(heap, text),
            text_of_ptr_size(heap, b"\xC0\xFE\xFE\xD0\x0D".as_ptr(), Bytes(5))
        ),
        0
    );
}
