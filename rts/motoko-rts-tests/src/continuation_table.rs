use crate::page_alloc::TestPageAlloc;

use motoko_rts::continuation_table::{
    continuation_count, recall_continuation, remember_continuation,
};
use motoko_rts::space::Space;
use motoko_rts::types::{Value, Words};

pub unsafe fn test() {
    println!("Testing continuation table ...");

    assert_eq!(continuation_count(), 0);

    const N: usize = 2000; // >256, to exercise `double_continuation_table`

    // Array will be doubled 3 times, so 256 + 512 + 1024 + 2048 = 3840 words, plus each array will
    // have 2 word header.
    // TODO: Implement large (more than page size) object allocation
    let heap = TestPageAlloc::new(Words(3848).to_bytes().as_usize());
    let mut space = Space::new(heap);

    let mut references: [u32; N] = [0; N];
    for i in 0..N {
        references[i] = remember_continuation(
            &mut space,
            Value::from_raw(((i as u32) << 2).wrapping_sub(1)),
        );
        assert_eq!(continuation_count(), (i + 1) as u32);
    }

    for i in 0..N / 2 {
        let c = recall_continuation(references[i]);
        assert_eq!(c.get_raw(), (i << 2).wrapping_sub(1) as u32);
        assert_eq!(continuation_count(), (N - i - 1) as u32);
    }

    for i in 0..N / 2 {
        references[i] = remember_continuation(
            &mut space,
            Value::from_raw(((i as u32) << 2).wrapping_sub(1)),
        );
        assert_eq!(continuation_count(), (N / 2 + i + 1) as u32);
    }

    for i in (0..N).rev() {
        assert_eq!(
            recall_continuation(references[i]).get_raw(),
            (i << 2).wrapping_sub(1) as u32,
        );
        assert_eq!(continuation_count(), i as u32);
    }
}
