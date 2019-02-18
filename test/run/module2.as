// check types and terms have separate namespaces
module X = { type x = Int; let x = 1; };
type x = X.x;
let x = X.x;
assert (x == 1);