import Prim "mo:prim";
import B "mo:stdlib/Buf";
import I "mo:stdlib/Iter";

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

func natArrayIter(elems:[Nat]) : I.Iter<Nat> = object {
  var pos = 0;
  let count = elems.len();
  public func next() : ?Nat {
    if (pos == count) { null } else {
      let elem = ?elems[pos];
      pos += 1;
      elem
    }
  }
};

func natVarArrayIter(elems:[var Nat]) : I.Iter<Nat> = object {
  var pos = 0;
  let count = elems.len();
  public func next() : ?Nat {
    if (pos == count) { null } else {
      let elem = ?elems[pos];
      pos += 1;
      elem
    }
  }
};

func natIterEq(a:I.Iter<Nat>, b:I.Iter<Nat>) : Bool {
   switch (a.next(), b.next()) {
     case (null, null) { true };
     case (?x, ?y) {
       if (x == y) { natIterEq(a, b) }
       else { false }
     };
     case (_, _) { false };
   }
};

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
  assert (c.len() == len);
  assert (c.toArray().len() == len);
  assert (natIterEq(c.iter(), natArrayIter(c.clone().toArray())));
  assert (c.toVarArray().len() == len);
  assert (natIterEq(c.iter(), natVarArrayIter(c.clone().toVarArray())));
};

// regression test: initially-empty buffers grow, element-by-element
{
  let c = B.Buf<Nat>(0);
  assert (c.toArray().len() == 0);
  assert (c.toVarArray().len() == 0);
  c.add(0);
  assert (c.toArray().len() == 1);
  assert (c.toVarArray().len() == 1);
  c.add(0);
  assert (c.toArray().len() == 2);
  assert (c.toVarArray().len() == 2);
};
