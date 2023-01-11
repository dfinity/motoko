use std::array::from_fn;

use crate::memory::TestMemory;

use motoko_rts::continuation_table::{
    continuation_count, recall_continuation, remember_continuation,
};
use motoko_rts::memory::alloc_blob;
use motoko_rts::types::{size_of, Array, Blob, Bytes, Value, Words};

pub unsafe fn test() {
    println!("Testing continuation table ...");

    assert_eq!(continuation_count(), 0);

    const N: usize = 2000; // >256, to exercise `double_continuation_table`

    // Array will be doubled 3 times, so 256 + 512 + 1024 + 2048 = 3840 words, plus each array will
    // have an array header. Also add the set of N pointers to empty blobs.
    let mut heap = TestMemory::new(Words(
        3840 + 4 * size_of::<Array>().to_bytes().as_u32()
            + N as u32 * size_of::<Blob>().to_bytes().as_u32(),
    ));

    let pointers: [Value; N] = from_fn(|_| alloc_blob(&mut heap, Bytes(0)));

    let mut references: [u32; N] = [0; N];
    for i in 0..N {
        references[i] = remember_continuation(&mut heap, pointers[i]);
        assert_eq!(continuation_count(), (i + 1) as u32);
    }

    for i in 0..N / 2 {
        let c = recall_continuation(references[i]);
        assert_eq!(c.get_ptr(), pointers[i].get_ptr());
        assert_eq!(continuation_count(), (N - i - 1) as u32);
    }

    for i in 0..N / 2 {
        references[i] = remember_continuation(&mut heap, pointers[i]);
        assert_eq!(continuation_count(), (N / 2 + i + 1) as u32);
    }

    for i in (0..N).rev() {
        assert_eq!(
            recall_continuation(references[i]).get_ptr(),
            pointers[i].get_ptr(),
        );
        assert_eq!(continuation_count(), i as u32);
    }
}
