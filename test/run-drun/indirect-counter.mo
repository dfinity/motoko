let a = actor {
  let aa = actor {
    var c = 1;
    public func inc() {
      c += 1;
      debugPrintNat(c)
    };
    public func debugPrint() {
      debugPrintNat(c)
    };
  };
  public func inc() { aa.inc() };
  public func debugPrint() { aa.debugPrint() };
};

a.inc();  //OR-CALL ingress inc "DIDL\x00\x00"
a.inc();  //OR-CALL ingress inc "DIDL\x00\x00"
a.inc();  //OR-CALL ingress inc "DIDL\x00\x00"
a.debugPrint() //OR-CALL ingress debugPrint "DIDL\x00\x00"

//SKIP comp
