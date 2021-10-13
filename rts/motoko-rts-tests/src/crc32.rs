use crate::page_alloc::TestPageAlloc;

use motoko_rts::principal_id::{base32_of_checksummed_blob, base32_to_blob};
use motoko_rts::space::Space;
use motoko_rts::text::{text_compare, text_of_ptr_size};
use motoko_rts::types::Bytes;

pub unsafe fn test() {
    println!("Testing crc32 ...");

    let page_alloc = TestPageAlloc::new();
    let mut space = Space::new(page_alloc);

    //
    // Encoding
    //

    let text = text_of_ptr_size(&mut space, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    let text1 = base32_of_checksummed_blob(&mut space, text);
    let text2 = text_of_ptr_size(
        &mut space,
        b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
        Bytes(32),
    );
    assert_eq!(text_compare(text1, text2), 0);

    let text = text_of_ptr_size(&mut space, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    assert_eq!(
        text_compare(
            base32_of_checksummed_blob(&mut space, text,),
            text_of_ptr_size(
                &mut space,
                b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
                Bytes(32)
            )
        ),
        0
    );

    //
    // Decoding
    //

    let text = text_of_ptr_size(&mut space, b"".as_ptr(), Bytes(0));
    assert_eq!(
        text_compare(
            base32_to_blob(&mut space, text),
            text_of_ptr_size(&mut space, b"".as_ptr(), Bytes(0))
        ),
        0
    );

    let text = text_of_ptr_size(&mut space, b"GEZDGNBVGY3TQOI".as_ptr(), Bytes(15));
    assert_eq!(
        text_compare(
            base32_to_blob(&mut space, text),
            text_of_ptr_size(&mut space, b"123456789".as_ptr(), Bytes(9))
        ),
        0
    );

    let text = text_of_ptr_size(
        &mut space,
        b"MFRGGZDFMZTWQ2LKNNWG23TPOA".as_ptr(),
        Bytes(26),
    );
    assert_eq!(
        text_compare(
            base32_to_blob(&mut space, text),
            text_of_ptr_size(&mut space, b"abcdefghijklmnop".as_ptr(), Bytes(16))
        ),
        0
    );

    let text = text_of_ptr_size(&mut space, b"em77e-bvlzu-aq".as_ptr(), Bytes(14));
    assert_eq!(
        text_compare(
            base32_to_blob(&mut space, text),
            text_of_ptr_size(
                &mut space,
                b"\x23\x3f\xf2\x06\xab\xcd\x01".as_ptr(),
                Bytes(7)
            )
        ),
        0
    );
}
