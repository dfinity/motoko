import B "buf.mo";

// test repeated growing
let a = B.Buf<Nat>(3);
for (i in range(0, 123)) {
  a.add(i);
};
for (i in range(0, 123)) {
  assert (a.get(i) == i);
};


// test repeated appending
let b = B.Buf<Nat>(3);
for (i in range(0, 123)) {
  b.append(a);
};

debugPrint(debug_show(a.toArray()));
debugPrint(debug_show(b.toArray()));
