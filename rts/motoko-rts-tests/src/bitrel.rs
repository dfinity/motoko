use motoko_rts::bitrel::{
    BitRel,//
};
use motoko_rts::types::{Value, Words};

pub unsafe fn test() {
    println!("Testing bitrel ...");

    const N : usize = 1024;

    let mut cache: [u32; N] = [0; N];

    const K : u32 = 64;

    assert_eq! (usize::BITS,32);
    for size1 in 1..K {
        for size2 in 1..K {
            let w = BitRel::words(size1, size2);
            let bitrel = BitRel { ptr: &mut cache[0], end: &mut cache[w as usize], size1: size1, size2: size2 };
            bitrel.init();
            for i in 0..size1 {
                for j in 0..size2 {
                    assert_eq!(bitrel.visited(true, i, j), false);
                    assert_eq!(bitrel.visited(false, j, i), false);
                    assert_eq!(bitrel.related(true, i, j), true);
                    assert_eq!(bitrel.related(false, j, i), true);
                    /*
                    bitrel.visit(true, i, j);
                    assert_eq!(bitrel.visited(true, i, j), true);

                    assert_eq!(bitrel.visited(false, j, i), false);
                    bitrel.visit(false, j, i);
                    assert_eq!(bitrel.visited(false, j, i), true);
                    
                    assert_eq!(bitrel.related(true, i, j), true);
                    bitrel.assume(true, i, j);
                    assert_eq!(bitrel.related(true, i, j), true);
                    bitrel.disprove(true, i, j);
                    assert_eq!(bitrel.related(true, i, j), false);
*/
                }
            }

        }

    }

    /*
    let mut heap = TestMemory::new(Words(3848));

    let mut references: [u32; N] = [0; N];
    for i in 0..N {
        references[i] = remember_continuation(
            &mut heap,
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
            &mut heap,
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
     */
}
