use motoko_rts::closure_table::{closure_count, recall_closure, remember_closure};
use motoko_rts::types::SkewedPtr;

pub unsafe fn test() {
    println!("Testing closure table ...");

    assert_eq!(closure_count(), 0);

    const N: usize = 2000; // >256, to exercise `double_closure_table`

    let mut references: [u32; N] = [0; N];
    for i in 0..N {
        references[i] = remember_closure(SkewedPtr((i << 2).wrapping_sub(1)));
        assert_eq!(closure_count(), (i + 1) as u32);
    }

    for i in 0..N / 2 {
        let c = recall_closure(references[i]);
        assert_eq!(c.0, (i << 2).wrapping_sub(1));
        assert_eq!(closure_count(), (N - i - 1) as u32);
    }

    for i in 0..N / 2 {
        references[i] = remember_closure(SkewedPtr((i << 2).wrapping_sub(1)));
        assert_eq!(closure_count(), (N / 2 + i + 1) as u32);
    }

    for i in (0..=N - 1).rev() {
        assert_eq!(recall_closure(references[i]).0, (i << 2).wrapping_sub(1));
        assert_eq!(closure_count(), i as u32);
    }

    println!("OK");
}
