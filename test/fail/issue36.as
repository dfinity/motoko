// AST-36: foo.bar.1.zap won't parse

type Z = { zap : Nat };
type B = { bar : (Int, Z) };

let inner : Z = new { zap = 42 };
let foo : B = new { bar = (25, inner) };

assert(foo.bar.1 == 25);

assert(foo.bar.2.zap == 42);

assert(foo.bar.0.zap == 42); // Dubious: parses, but type error

assert((0,((1,1,2), (3,5), 8), 12).2.2.2 == 5);

// Slight imbalance: between DOT and ID we can have whitespace...

assert(foo. bar .2 . zap == 42);

// but not between DOT and NUM:

// assert(foo.bar. 2.zap == 42) // Error

// N.B.: We did not change the FLOAT syntax:

let (f, g, h, i) : (Float, Float, Float, Float) = (1., 1.7, 1.8e-4, 0x644.)
