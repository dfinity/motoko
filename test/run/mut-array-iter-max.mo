import Prim "mo:⛔";
// test we can iterate over vals and keys of largest mutable array
let max_size = 2**29; // maximum array size
let a = Prim.Array_init<Nat>(max_size, 666);
var c = 0;
for (v in a.vals()) {
 assert v == 666; c += 1;
}
;
assert c == max_size;
Prim.debugPrint(debug_show c);
var d = 0;
for (k in a.keys()) {
  assert k == d; d += 1;
};
assert d == max_size;
Prim.debugPrint(debug_show d);

// Prim.Array_init<Nat>(max_size+1,0); // should trap, but mangled stacktrace is noisy for testing

//SKIP run
//SKIP run-ir
//SKIP run-low
