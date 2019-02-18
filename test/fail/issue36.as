// foo.bar.1.zap won't parse

type Z = { zap : Nat };
type B = { bar : (Int, Z) };

let inner : Z = new { zap = 42 };
let foo : B = new { bar = (25, inner) };

assert(foo.bar.2.zap == 42)
