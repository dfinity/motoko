import Prim "mo:prim";
actor a {
  var c : Int = 0;
  public func inci(n : Int) : () {
    c += n;
    Prim.debugPrintInt(c);
  };
  public func incn(n : Nat) : () {
    c += n;
    Prim.debugPrintInt(c);
  };
  public func incnn(n1 : Nat, n2 : Nat) : () {
    c += n1 + n2;
    Prim.debugPrintInt(c);
  };
  public func incnested(n1 : Nat, (n2 : Nat, n3 : Nat)) : () {
    c += n1 + n2 + n3;
    Prim.debugPrintInt(c);
  };
  public func incarray(a : [Nat]) : () {
    for (i in a.vals()) { c += i };
    Prim.debugPrintInt(c);
  };
  public func incopt(a : ?Nat) : () {
    switch a {
      case null { c += 1000000 };
      case (?a) { c += a };
    };
    Prim.debugPrintInt(c);
  };
  public func increcord(a : { x : Nat; y : Nat }) : () {
    c += a.x;
    c += a.y;
    Prim.debugPrintInt(c);
  };
  public func incVariant(v : { #foo : Nat; #bar : Nat }) {
    c += (switch v {
      case (#foo n) n;
      case (#bar n) n;
    });
    Prim.debugPrintInt(c);
  };
  public func printCounter() {
    Prim.debugPrintInt(c);
  };
  public func printLabeled(l:Text) {
    Prim.debugPrint l;
    Prim.debugPrintInt(c);
  };
  public func printLabeledOpt(lo:?Text) {
    let ?l = lo;
    Prim.debugPrint l;
    Prim.debugPrintInt(c);
  };
  public func incwords(w8 : Word8, w16 : Word16, w32 : Word32, w64 : Word64) : () {
    c += Prim.word8ToInt(w8);
    c += Prim.word16ToInt(w16);
    c += Prim.word32ToInt(w32);
    c += Prim.word64ToInt(w64);
    Prim.debugPrintInt(c);
  };
  public func incnats(n8 : Nat8, n16 : Nat16, n32 : Nat32, n64 : Nat64) : () {
    c += Prim.nat8ToNat(n8);
    c += Prim.nat16ToNat(n16);
    c += Prim.nat32ToNat(n32);
    c += Prim.nat64ToNat(n64);
    Prim.debugPrintInt(c);
  };
  public func incints(i8 : Int8, i16 : Int16, i32 : Int32, i64 : Int64) : () {
    c += Prim.int8ToInt(i8);
    c += Prim.int16ToInt(i16);
    c += Prim.int32ToInt(i32);
    c += Prim.int64ToInt(i64);
    Prim.debugPrintInt(c);
  };
  var f : Float = 0.0;
  public func incfloat(f64 : Float) : () {
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

  public func go() : () {
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
    a.increcord({x = 15; y = 16});
    a.increcord({x = 17; y = 18; z = 19});
    a.incVariant(#foo 20);
    a.incVariant(#bar 20);
    a.printCounter();
    a.printLabeled("Foo1: ");
    a.printLabeled("μεταγράψτε: ");
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
    a.incfloat(42.0);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
