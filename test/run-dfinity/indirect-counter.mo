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

a.inc();
a.inc();
a.inc();
a.debugPrint()
