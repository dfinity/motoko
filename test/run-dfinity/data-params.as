let a = actor {
  private var c : Int = 0;
  inci(n : Int) : () {
    c += n;
    printInt(c);
    print("\n");
  };
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
  incVariant(v : { #foo : Nat; #bar : Nat }) {
    c += (switch v {
      case (#foo n) n;
      case (#bar n) n;
    });
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
  incwords(w16 : Word16, w32 : Word32, w64 : Word64) : () {
    c += word16ToInt(w16);
    c += word32ToInt(w32);
    c += word64ToInt(w64);
    printInt(c);
    print("\n");
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
a.incVariant(#foo 20);
a.incVariant(#bar 20);
a.printCounter();
a.printLabeled("Foo1: ");
a.printLabeledOpt(?"Foo2: ");
a.incn(10000000000000);
a.inci(10000000000000);
a.inci(-20000000000000);
a.incwords(1,2,3);
a.incwords(-1,-2,-3);
a.incn(2**100);
a.inci(2**100);
a.inci(-(2**101));
