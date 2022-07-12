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
ignore ({ a = 8 : Int; b = 'X' in b and m and m and b } : { a : Int; b : Char })
