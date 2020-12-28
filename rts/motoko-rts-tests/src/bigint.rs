use motoko_rts::bigint::{bigint_eq, bigint_leb128_size, bigint_mul, bigint_of_word32, bigint_pow};
use motoko_rts::types::SkewedPtr;

pub unsafe fn test() {
    println!("Testing BigInt ...");

    assert!(bigint_eq(
        bigint_pow(bigint_of_word32(70), bigint_of_word32(32)),
        bigint_mul(
            bigint_pow(bigint_of_word32(70), bigint_of_word32(31)),
            bigint_of_word32(70)
        )
    ));
}
