import Prim "mo:prim";
let _ = - Prim.intToInt64 (- 2 ** 63); // this should trap
