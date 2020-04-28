import Prim "mo:prim";

class range(x : Nat, y : Nat) {
  var i = x;
  public func next() : ?Nat { if (i > y) null else {let j = i; i += 1; ?j} };
};


actor a {
  public func go()  : async () {
    {
      var i = 0;
      i := 0;
      for (j in range(0, 10)) {
       Prim.debugPrintNat(j);
       assert(j == i);
       await (async (i += 1));
      };
      assert(i == 11);
    };
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
