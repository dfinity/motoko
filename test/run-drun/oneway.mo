import Prim "mo:prim";
actor a {
  // test that oneways can locally try/throw
  public func oneway() : () {
    Prim.debugPrint "1";
    try {
      throw (Prim.error("Error"));
      Prim.debugPrint "unreachable";
    }
    catch e { Prim.debugPrint "2"};
  };

  // test that oneways can locally try/throw
  // using `=` syntax
  public func onewayAlt() : () =
    ignore (
      (async {
        Prim.debugPrint "3";
        try {
          throw (Prim.error("Error"));
          Prim.debugPrint "unreachable";
        }
        catch e { Prim.debugPrint "4"};
      }) : async ()
    );


  // test that throws from oneways are silently discarded (because replies are eager)
  public func discard() : () {
    Prim.debugPrint "5";
    throw (Prim.error("ignored"));
    Prim.debugPrint "unreachable";
  };

  // test that throws from oneways are silently discarded (because replies are eager)
  // using `=` syntax
  public func discardAlt() : () =
    ignore (
      (async {
        Prim.debugPrint "6";
        throw (Prim.error("ignored"));
        Prim.debugPrint "unreachable";
      }) : async ()
    );

  // TODO test await and calls to shared functions

  public func go() {
    Prim.debugPrint("A");
    oneway();
    Prim.debugPrint("B");
    onewayAlt();
    Prim.debugPrint("C");
    discard();
    Prim.debugPrint("D");
    discardAlt();
    Prim.debugPrint("E");
  };
};
a.go(); //OR-CALL ingress go 0x4449444C0000

// do not run with drun, hit by nondeterminism in https://dfinity.atlassian.net/browse/DFN-1269
//SKIP comp
