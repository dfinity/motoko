import Prim "mo:prim";
actor a {
  public func go() = ignore async {
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
