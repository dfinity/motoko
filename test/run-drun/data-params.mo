import Prim "mo:prim";
actor a {
  flexible var c : Int = 0;
  public func inci(n : Int) : async () {
    c += n;
    Prim.debugPrintInt(c);
  };
  public func incn(n : Nat) : async () {
    c += n;
    Prim.debugPrintInt(c);
  };
  public func incnn(n1 : Nat, n2 : Nat) : async () {
    c += n1 + n2;
    Prim.debugPrintInt(c);
  };
  public func incnested(n1 : Nat, (n2 : Nat, n3 : Nat)) : async () {
    c += n1 + n2 + n3;
    Prim.debugPrintInt(c);
  };
  public func incarray(a : [Nat]) : async () {
    for (i in a.vals()) { c += i };
    Prim.debugPrintInt(c);
  };
  public func incopt(a : ?Nat) : async () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    Prim.debugPrintInt(c);
  };
  public func increcord(a : { x : Nat; y : Nat }) : async () {
    c += a.x;
    c += a.y;
    Prim.debugPrintInt(c);
  };
  public func incVariant(v : { #foo : Nat; #bar : Nat }) : async () {
    c += (switch v {
      case (#foo n) n;
      case (#bar n) n;
    });
    Prim.debugPrintInt(c);
  };
  public func printCounter() : async () {
    Prim.debugPrintInt(c);
  };
  public func printLabeled(l:Text) : async () {
    Prim.debugPrint l;
    Prim.debugPrintInt(c);
  };
  public func printLabeledOpt(lo:?Text) : async () {
    let ?l = lo;
    Prim.debugPrint l;
    Prim.debugPrintInt(c);
  };
  public func incwords(w8 : Word8, w16 : Word16, w32 : Word32, w64 : Word64) : async () {
    c += Prim.word8ToInt(w8);
    c += Prim.word16ToInt(w16);
    c += Prim.word32ToInt(w32);
    c += Prim.word64ToInt(w64);
    Prim.debugPrintInt(c);
  };
  public func incnats(n8 : Nat8, n16 : Nat16, n32 : Nat32, n64 : Nat64) : async () {
    c += Prim.nat8ToNat(n8);
    c += Prim.nat16ToNat(n16);
    c += Prim.nat32ToNat(n32);
    c += Prim.nat64ToNat(n64);
    Prim.debugPrintInt(c);
  };
  public func incints(i8 : Int8, i16 : Int16, i32 : Int32, i64 : Int64) : async () {
    c += Prim.int8ToInt(i8);
    c += Prim.int16ToInt(i16);
    c += Prim.int32ToInt(i32);
    c += Prim.int64ToInt(i64);
    Prim.debugPrintInt(c);
  };
  flexible var f : Float = 0.0;
  public func incfloat(f64 : Float) : async () {
    f += f64;
    assert(f == 42.0);
    Prim.debugPrint(debug_show(f));
    c += Prim.int64ToInt(Prim.floatToInt64(f));
    Prim.debugPrintInt(c);
    f += Prim.int64ToFloat(Prim.intToInt64(c));
    Prim.debugPrint(debug_show(f));
    f += Prim.int64ToFloat(Prim.intToInt64(-c));
    Prim.debugPrint(debug_show(f));
  };

  public func go() : async () {
    await a.incn(0);
    await a.incn(1);
    await a.incn(2);
    await a.incn(3);
    await a.incn(4);
    await a.incn(1000);
    await a.incnn(5,6);
    await a.incnn(2000,3000);
    await a.incnested(7,(8,9));
    await a.incarray([10,11,12,13]);
    await a.incopt(null);
    await a.incopt(?14);
    await a.increcord({x = 15; y = 16});
    await a.increcord({x = 17; y = 18; z = 19});
    await a.incVariant(#foo 20);
    await a.incVariant(#bar 20);
    await a.printCounter();
    await a.printLabeled("Foo1: ");
    await a.printLabeled("μεταγράψτε: ");
    await a.printLabeledOpt(?"Foo2: ");
    await a.incn(10000000000000);
    await a.inci(10000000000000);
    await a.inci(-20000000000000);
    await a.incwords(1,2,3,4);
    await a.incwords(-1,-2,-3,-4);
    await a.incnats(1,2,3,4);
    await a.incints(1,2,3,4);
    await a.incints(-1,-2,-3,-4);
    await a.incn(2**100);
    await a.inci(2**100);
    await a.inci(-(2**101));
    await a.inci(-2**30); // lowest compact
    await a.inci(2**30 - 1); // highest compact
    await a.inci(0);
    await a.inci(1);
    await a.inci(42);
    await a.inci(-42);
    await a.incn(2**31 - 1); // highest compact
    await a.incfloat(42.0);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
