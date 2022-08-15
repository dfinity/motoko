import Prim "mo:⛔";

// synthesis
let b = { b = 6 };
module m { public let b = 6 };
Prim.debugPrint (debug_show { b with a = 8 });
Prim.debugPrint (debug_show { b and m with b = 8 });
Prim.debugPrint (debug_show { { b = 6; c = "C" } with a = 8 });
Prim.debugPrint (debug_show { { c = 'C'; d = "D" } with a = 8; b = 6 });

// analysis
ignore ({ b with a = 8 } : { a : Nat });
ignore ({ b with a = 8 } : { a : Nat; b : Nat });
ignore ({ b and m with a = 8 : Int; b = 'X'} : { a : Int; b : Char });
ignore ({ b and m and m with a = 8 : Int; b = 'X' } : { a : Int; b : Char });
ignore ({ b and m and m and b with a = 8 : Int; b = 'X' } : { a : Int; b : Char });

// var fields
let c = { var c = 25 };

let d = { c with var c = c.c };
c.c += 1;
assert c.c == d.c + 1;

let e = { c with e = 42 };
c.c += 1;
assert c.c == e.c;

// methods closing over var fields
let c0 = object {
    public var c : Int = 0;
    public func incr() = c += 1
};

let c1 = { c0 with d = 2; };

assert c1.c == 0;
c1.incr();
assert c1.c == 1;
assert c0.c == 1;

// this is checking that the interpreter doesn't consider
// dynamic fields not present in the static type

let r : {} = { l = 0 };
let s = { l = true };
let t = { r and s };
assert t.l;
