import Prim "mo:⛔";

actor a {

  public func f() : async () {
  };

  public func go() : async () {
    try {
      var n = 0;
      await async ();
      while (n < 1000) {
        Prim.debugPrint(debug_show n);
        ignore f();
        n += 1;
      }
    } catch e {
      Prim.debugPrint("caught" # Prim.errorMessage(e));
    }
  };

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//await a.go(); //OR-CALL ingress go "DIDL\x00\x00"