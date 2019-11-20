import ArrayBuf "arrayBuf.mo";

let a = ArrayBuf.Buf<Nat>(3);
for (i in range(0, 123)) {
  a.add(i);
};
for (i in range(0, 123)) {
  assert (a.get(i) == i);
}

