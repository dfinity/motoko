use motoko_rts::heap::Heap;
use motoko_rts::principal_id::{base32_of_checksummed_blob, base32_to_blob};
use motoko_rts::text::{text_compare, text_of_ptr_size};
use motoko_rts::types::Bytes;

pub unsafe fn test<H: Heap>(heap: &mut H) {
    println!("Testing crc32 ...");

    //
    // Encoding
    //

    let text = text_of_ptr_size(heap, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    let text1 = base32_of_checksummed_blob(heap, text);
    let text2 = text_of_ptr_size(
        heap,
        b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
        Bytes(32),
    );
    assert_eq!(text_compare(text1, text2), 0);

    let text = text_of_ptr_size(heap, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    assert_eq!(
        text_compare(
            base32_of_checksummed_blob(heap, text,),
            text_of_ptr_size(
                heap,
                b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
                Bytes(32)
            )
        ),
        0
    );

    //
    // Decoding
    //

    let text = text_of_ptr_size(heap, b"".as_ptr(), Bytes(0));
    assert_eq!(
        text_compare(
            base32_to_blob(heap, text),
            text_of_ptr_size(heap, b"".as_ptr(), Bytes(0))
        ),
        0
    );

    let text = text_of_ptr_size(heap, b"GEZDGNBVGY3TQOI".as_ptr(), Bytes(15));
    assert_eq!(
        text_compare(
            base32_to_blob(heap, text),
            text_of_ptr_size(heap, b"123456789".as_ptr(), Bytes(9))
        ),
        0
    );

    let text = text_of_ptr_size(heap, b"MFRGGZDFMZTWQ2LKNNWG23TPOA".as_ptr(), Bytes(26));
    assert_eq!(
        text_compare(
            base32_to_blob(heap, text),
            text_of_ptr_size(heap, b"abcdefghijklmnop".as_ptr(), Bytes(16))
        ),
        0
    );

    let text = text_of_ptr_size(heap, b"em77e-bvlzu-aq".as_ptr(), Bytes(14));
    assert_eq!(
        text_compare(
            base32_to_blob(heap, text),
            text_of_ptr_size(heap, b"\x23\x3f\xf2\x06\xab\xcd\x01".as_ptr(), Bytes(7))
        ),
        0
    );
}
