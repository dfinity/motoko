import Prim "mo:prim";
import B "mo:stdlib/buf";
import I "mo:stdlib/iter";

// test repeated growing
let a = B.Buf<Nat>(3);
for (i in I.range(0, 123)) {
  a.add(i);
};
for (i in I.range(0, 123)) {
  assert (a.get(i) == i);
};


// test repeated appending
let b = B.Buf<Nat>(3);
for (i in I.range(0, 123)) {
  b.append(a);
};

Prim.debugPrint(debug_show(a.toArray()));
Prim.debugPrint(debug_show(b.toArray()));
