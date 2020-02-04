import Prim "mo:prim";
let _ = - Prim.intToInt16 (- 2 ** 15); // this should trap
