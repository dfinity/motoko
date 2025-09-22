import Prim "mo:prim";

let ?o1 = from_candid(to_candid({x = (5:Nat32)})) :?({x: ?Nat32});
let ?o2 = from_candid(to_candid({x = (5:Nat32)})) :?({x:??Nat32});

Prim.debugPrint (debug_show {o1;o2});

assert o1.x == ?5; // succeeds
assert o2.x == ??5; // fails (but should now succeed)
//SKIP run
//SKIP run-ir
//SKIP run-low
