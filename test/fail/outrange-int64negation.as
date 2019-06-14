let _ = - intToInt64 (- 2 ** 61); // this should work, but traps (BigNum bug?)
let _ = - intToInt64 (- 2 ** 63 + 1); // FIXME: this should work, but traps (BigNum bug?)

// let _ = - intToInt64 (- 2 ** 63); // this should trap
