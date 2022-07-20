import Prim "mo:⛔";

// synthesis
let b = { b = 6 };
module m { public let b = 6 };
Prim.debugPrint (debug_show { b | a = 8 });
Prim.debugPrint (debug_show { b and m | b = 8 });
Prim.debugPrint (debug_show { { b = 6; c = "C" } | a = 8 });
Prim.debugPrint (debug_show { { c = 'C'; d = "D" } | a = 8; b = 6 });

// analysis
ignore ({ b | a = 8 } : { a : Nat });
ignore ({ b | a = 8 } : { a : Nat; b : Nat });
ignore ({ b and m | a = 8 : Int; b = 'X'} : { a : Int; b : Char });
ignore ({ b and m and m | a = 8 : Int; b = 'X' } : { a : Int; b : Char });
ignore ({ b and m and m and b | a = 8 : Int; b = 'X' } : { a : Int; b : Char });

// var fields
let c = { var c = 25 };

let d = { c | var c = c.c };
c.c += 1;
assert c.c == d.c + 1;

let e = { c | e = 42 };
c.c += 1;
assert c.c == e.c;

// this is checking that the interpreter doesn't consider
// dynamic fields not present in the static type

let r : {} = { l = 0 };
let s = { l = true };
let t = { r and s };
assert t.l;
