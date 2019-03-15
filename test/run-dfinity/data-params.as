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
  printLabeledOpt(?l:?Text) {
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
a.printLabeled("Foo1: ");
a.printLabeledOpt(?"Foo2: ");
// a.readCounter(func (n : Nat) = { printInt n; print("\n") });
a.incn(10000000000000);


let w32 = actor {
  private var c : Word32 = 0;
  incn(n : Word32) : () {
    c += n;
    printInt(word32ToInt(c));
    print("\n");
  };
  incnn(n1 : Word32, n2 : Word32) : () {
    c += n1 + n2;
    printInt(word32ToInt(c));
    print("\n");
  };
  incnested(n1 : Word32, (n2 : Word32, n3 : Word32)) : () {
    c += n1 + n2 + n3;
    printInt(word32ToInt(c));
    print("\n");
  };
  incarray(a : [Word32]) : () {
    for (i in a.vals()) { c += i };
    printInt(word32ToInt(c));
    print("\n");
  };
  incopt(a : ?Word32) : () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    printInt(word32ToInt(c));
    print("\n");
  };
  increcord(a : shared { x : Word32; y : Word32 }) : () {
    c += a.x;
    c += a.y;
    printInt(word32ToInt(c));
    print("\n");
  };
  printCounter() {
    printInt(word32ToInt(c));
    print("\n");
  };
  printLabeled(l:Text) {
    print l;
    printInt(word32ToInt(c));
    print("\n");
  };
  printLabeledOpt(?l:?Text) {
    print l;
    printInt(word32ToInt(c));
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
w32.printLabeled("Foo1: ");
w32.printLabeledOpt(?"Foo2: ");



let w16 = actor {
  private var c : Word16 = 0;
  incn(n : Word16) : () {
    c += n;
    printInt(word16ToInt(c));
    print("\n");
  };
  incnn(n1 : Word16, n2 : Word16) : () {
    c += n1 + n2;
    printInt(word16ToInt(c));
    print("\n");
  };
  incnested(n1 : Word16, (n2 : Word16, n3 : Word16)) : () {
    c += n1 + n2 + n3;
    printInt(word16ToInt(c));
    print("\n");
  };
  incarray(a : [Word16]) : () {
    for (i in a.vals()) { c += i };
    printInt(word16ToInt(c));
    print("\n");
  };
  incopt(a : ?Word16) : () {
    switch a {
      case null { c += 10000 };
      case (?a) { c += a };
    };
    printInt(word16ToInt(c));
    print("\n");
  };
  increcord(a : shared { x : Word16; y : Word16 }) : () {
    c += a.x;
    c += a.y;
    printInt(word16ToInt(c));
    print("\n");
  };
  printCounter() {
    printInt(word16ToInt(c));
    print("\n");
  };
  printLabeled(l:Text) {
    print l;
    printInt(word16ToInt(c));
    print("\n");
  };
  printLabeledOpt(?l:?Text) {
    print l;
    printInt(word16ToInt(c));
    print("\n");
  };
  readCounter(f : shared Word16 -> ()) : () {
    f(c);
  };
};


w16.incn(1);
w16.incn(2);
w16.incn(3);
w16.incn(4);
w16.incn(1000);
w16.incnn(5,6);
w16.incnn(2000,3000);
w16.incnested(7,(8,9));
w16.incarray([10,11,12,13]);
w16.incopt(null);
w16.incopt(?14);
w16.increcord(shared {x = 15 : Word16; y = 16 : Word16});
w16.increcord(shared {x = 17 : Word16; y = 18 : Word16; z = 19 : Word16});
w16.printCounter();
w16.printLabeled("Foo1: ");
w16.printLabeledOpt(?"Foo2: ");
