actor a {
  // test that oneways can locally try/throw
  public func oneway() : () {
    ignore (
      async {
        debugPrint "1";
        try {
          throw (error("Error"));
          debugPrint "unreachable";
        }
        catch e { debugPrint "2"};
      }
    )
  };

  // test that oneways can locally try/throw
  // using `=` syntax
  public func onewayAlt() : () =
    ignore (
      async {
        debugPrint "3";
        try {
          throw (error("Error"));
          debugPrint "unreachable";
        }
        catch e { debugPrint "4"};
      }
    );


  // test that throws from oneways are silently discarded (because replies are eager)
  public func discard() : () {
    ignore (
      async {
        debugPrint "5";
        throw (error("ignored"));
        debugPrint "unreachable";
      }
    )
  };

  // test that throws from oneways are silently discarded (because replies are eager)
  // using `=` syntax
  public func discardAlt() : () =
    ignore (
      async {
        debugPrint "6";
        throw (error("ignored"));
        debugPrint "unreachable";
      }
    );

  // TODO test await and calls to shared functions

  public func go() = ignore async {
    debugPrint("A");
    oneway();
    debugPrint("B");
    onewayAlt();
    debugPrint("C");
    discard();
    debugPrint("D");
    discardAlt();
    debugPrint("E");
  };
};
a.go(); //OR-CALL ingress go 0x4449444C0000
