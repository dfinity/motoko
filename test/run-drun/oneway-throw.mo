actor a {
  var committed = false;
  public func throws() : () = ignore async {
    debugPrint "throws()";
    committed := true;
    throw (error("ignored"));
    debugPrint "unreachable";
  };
  public func ping() : async () {
    debugPrint "ping()";
  };
  public func go() = ignore async {
    debugPrint "go1";
    throws();
    await ping(); // in-order delivery guarantees that throw ran
    debugPrint "go2";
    assert(committed);
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
