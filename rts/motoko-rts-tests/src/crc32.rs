use crate::memory::{initialize_test_memory, reset_test_memory};

use motoko_rts::principal_id::{base32_of_checksummed_blob, base32_to_blob, blob_of_ptr_size};
use motoko_rts::text::{blob_compare, text_compare, text_of_ptr_size};
use motoko_rts::types::Bytes;

pub unsafe fn test() {
    println!("Testing crc32 ...");

    let mut heap = initialize_test_memory();

    //
    // Encoding
    //

    let text = text_of_ptr_size(&mut heap, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    let text1 = base32_of_checksummed_blob(&mut heap, text);
    let text2 = text_of_ptr_size(
        &mut heap,
        b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
        Bytes(32),
    );
    assert_eq!(text_compare(text1, text2), 0);

    let text = text_of_ptr_size(&mut heap, b"abcdefghijklmnop".as_ptr(), Bytes(16));
    assert_eq!(
        text_compare(
            base32_of_checksummed_blob(&mut heap, text,),
            text_of_ptr_size(
                &mut heap,
                b"SQ5MBE3BMJRWIZLGM5UGS2TLNRWW433Q".as_ptr(),
                Bytes(32)
            )
        ),
        0
    );

    //
    // Decoding
    //

    let text = text_of_ptr_size(&mut heap, b"".as_ptr(), Bytes(0));
    assert_eq!(
        blob_compare(
            base32_to_blob(&mut heap, text),
            blob_of_ptr_size(&mut heap, b"".as_ptr(), Bytes(0))
        ),
        0
    );

    let text = text_of_ptr_size(&mut heap, b"GEZDGNBVGY3TQOI".as_ptr(), Bytes(15));
    assert_eq!(
        blob_compare(
            base32_to_blob(&mut heap, text),
            blob_of_ptr_size(&mut heap, b"123456789".as_ptr(), Bytes(9))
        ),
        0
    );

    let text = text_of_ptr_size(&mut heap, b"MFRGGZDFMZTWQ2LKNNWG23TPOA".as_ptr(), Bytes(26));
    assert_eq!(
        blob_compare(
            base32_to_blob(&mut heap, text),
            blob_of_ptr_size(&mut heap, b"abcdefghijklmnop".as_ptr(), Bytes(16))
        ),
        0
    );

    let text = text_of_ptr_size(&mut heap, b"em77e-bvlzu-aq".as_ptr(), Bytes(14));
    assert_eq!(
        blob_compare(
            base32_to_blob(&mut heap, text),
            blob_of_ptr_size(
                &mut heap,
                b"\x23\x3f\xf2\x06\xab\xcd\x01".as_ptr(),
                Bytes(7)
            )
        ),
        0
    );

    reset_test_memory();
}
