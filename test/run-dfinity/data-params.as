let a = actor {
  private var c = 0;
  incn(n : Nat) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  incnn(n1 : Nat, n2 : Nat) : () {
    c += n1 + n2;
    printInt(c);
    print("\n");
  };
  incnested(n1 : Nat, (n2 : Nat, n3 : Nat)) : () {
    c += n1 + n2 + n3;
    printInt(c);
    print("\n");
  };
  incarray(a : [Nat]) : () {
    for (i in a.vals()) { c += i };
    printInt(c);
    print("\n");
  };
  incopt(a : ?Nat) : () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    printInt(c);
    print("\n");
  };
  increcord(a : shared { x : Nat; y : Nat }) : () {
    c += a.x;
    c += a.y;
    printInt(c);
    print("\n");
  };
  printCounter() {
    printInt(c);
    print("\n");
  };
  printLabeled(l:Text) {
    print l;
    printInt(c);
    print("\n");
  };
  readCounter(f : shared Nat -> ()) : () {
    f(c);
  };
};


a.incn(1);
a.incn(2);
a.incn(3);
a.incn(4);
a.incn(1000);
a.incnn(5,6);
a.incnn(2000,3000);
a.incnested(7,(8,9));
a.incarray([10,11,12,13]);
a.incopt(null);
a.incopt(?14);
a.increcord(shared {x = 15; y = 16});
a.increcord(shared {x = 17; y = 18; z = 19});
a.printCounter();
a.printLabeled("Foo: ");
// a.readCounter(func (n : Nat) = { printInt n; print("\n") });


let w32 = actor {
  private var c : Word32 = 0;
  incn(n : Word32) : () {
    c += n;
    printW32(c);
    print("\n");
  };
  incnn(n1 : Word32, n2 : Word32) : () {
    c += n1 + n2;
    printW32(c);
    print("\n");
  };
  incnested(n1 : Word32, (n2 : Word32, n3 : Word32)) : () {
    c += n1 + n2 + n3;
    printW32(c);
    print("\n");
  };
  incarray(a : [Word32]) : () {
    for (i in a.vals()) { c += i };
    printW32(c);
    print("\n");
  };
  incopt(a : ?Word32) : () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    printW32(c);
    print("\n");
  };
  increcord(a : shared { x : Word32; y : Word32 }) : () {
    c += a.x;
    c += a.y;
    printW32(c);
    print("\n");
  };
  printW32(x : Word32) {
    printInt(word32ToInt x);
  };
  printCounter() {
    printW32(c);
    print("\n");
  };
  printLabeled(l:Text) {
    print l;
    printW32(c);
    print("\n");
  };
  readCounter(f : shared Word32 -> ()) : () {
    f(c);
  };
};


w32.incn(1);
w32.incn(2);
w32.incn(3);
w32.incn(4);
w32.incn(1000);
w32.incnn(5,6);
w32.incnn(2000,3000);
w32.incnested(7,(8,9));
w32.incarray([10,11,12,13]);
w32.incopt(null);
w32.incopt(?14);
w32.increcord(shared {x = 15 : Word32; y = 16 : Word32});
w32.increcord(shared {x = 17 : Word32; y = 18 : Word32; z = 19 : Word32});
w32.printCounter();
w32.printLabeled("Foo: ");
