import Prim "mo:⛔";

// synthesis
let b = { b = 6 };
module m { public let b = 6 };
Prim.debugPrint (debug_show { a = 8 in b });
Prim.debugPrint (debug_show { b = 8 in b and m });
Prim.debugPrint (debug_show { a = 8 in { b = 6; c = "C" } });
Prim.debugPrint (debug_show { a = 8; b = 6 in { c = 'C'; d = "D" } });

// analysis
ignore ({ a = 8 in b } : { a : Nat });
ignore ({ a = 8 in b } : { a : Nat; b : Nat });
ignore ({ a = 8 : Int; b = 'X' in b and m } : { a : Int; b : Char });
ignore ({ a = 8 : Int; b = 'X' in b and m and m } : { a : Int; b : Char });
ignore ({ a = 8 : Int; b = 'X' in b and m and m and b } : { a : Int; b : Char });

// var fields
let c = { var c = 25 };

let d = { var c = c.c in c };
c.c += 1;
assert c.c == d.c + 1;

let e = { e = 42 in c };
c.c += 1;
assert c.c == e.c;
