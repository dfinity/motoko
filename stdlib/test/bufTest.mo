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

// regression test: buffers with extra space are converted to arrays of the correct length
{
  let bigLen = 100;
  let len = 3;
  let c = B.Buf<Nat>(bigLen);
  assert (len < bigLen);
  for (i in I.range(0, len - 1)) {
    Prim.debugPrint(debug_show(i));
    c.add(i);
  };
  Prim.debugPrint(debug_show(c.len()));
  assert (c.len() == len);
  Prim.debugPrint(debug_show(c.len()));
  assert (c.toArray().len() == len);
  Prim.debugPrint(debug_show(c.len()));
  assert (c.toVarArray().len() == len);
}
