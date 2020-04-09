import Prim "mo:prim";
let _ = - Prim.intToInt32 (- 2 ** 31); // this should trap
