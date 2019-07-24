let a = actor {
  var c : Int = 0;
  public func inci(n : Int) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  public func incn(n : Nat) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  public func incnn(n1 : Nat, n2 : Nat) : () {
    c += n1 + n2;
    printInt(c);
    print("\n");
  };
  public func incnested(n1 : Nat, (n2 : Nat, n3 : Nat)) : () {
    c += n1 + n2 + n3;
    printInt(c);
    print("\n");
  };
  public func incarray(a : [Nat]) : () {
    for (i in a.vals()) { c += i };
    printInt(c);
    print("\n");
  };
  public func incopt(a : ?Nat) : () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    printInt(c);
    print("\n");
  };
  public func increcord(a : { x : Nat; y : Nat }) : () {
    c += a.x;
    c += a.y;
    printInt(c);
    print("\n");
  };
  public func incVariant(v : { #foo : Nat; #bar : Nat }) {
    c += (switch v {
      case (#foo n) n;
      case (#bar n) n;
    });
    printInt(c);
    print("\n");
  };
  public func printCounter() {
    printInt(c);
    print("\n");
  };
  public func printLabeled(l:Text) {
    print l;
    printInt(c);
    print("\n");
  };
  public func printLabeledOpt(?l:?Text) {
    print l;
    printInt(c);
    print("\n");
  };
  public func incwords(w8 : Word8, w16 : Word16, w32 : Word32, w64 : Word64) : () {
    c += word8ToInt(w8);
    c += word16ToInt(w16);
    c += word32ToInt(w32);
    c += word64ToInt(w64);
    printInt(c);
    print("\n");
  };
  public func incnats(n8 : Nat8, n16 : Nat16, n32 : Nat32, n64 : Nat64) : () {
    c += nat8ToNat(n8);
    c += nat16ToNat(n16);
    c += nat32ToNat(n32);
    c += nat64ToNat(n64);
    printInt(c);
    print("\n");
  };
  public func incints(i8 : Int8, i16 : Int16, i32 : Int32, i64 : Int64) : () {
    c += int8ToInt(i8);
    c += int16ToInt(i16);
    c += int32ToInt(i32);
    c += int64ToInt(i64);
    printInt(c);
    print("\n");
  };
};


a.incn(0);
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
a.increcord(new {x = 15; y = 16});
a.increcord(new {x = 17; y = 18; z = 19});
a.incVariant(#foo 20);
a.incVariant(#bar 20);
a.printCounter();
a.printLabeled("Foo1: ");
a.printLabeledOpt(?"Foo2: ");
a.incn(10000000000000);
a.inci(10000000000000);
a.inci(-20000000000000);
a.incwords(1,2,3,4);
a.incwords(-1,-2,-3,-4);
a.incnats(1,2,3,4);
a.incints(1,2,3,4);
a.incints(-1,-2,-3,-4);
a.incn(2**100);
a.inci(2**100);
a.inci(-(2**101));
a.inci(-2**30); // lowest compact
a.inci(2**30 - 1); // highest compact
a.inci(0);
a.inci(1);
a.inci(42);
a.inci(-42);
a.incn(2**31 - 1); // highest compact
