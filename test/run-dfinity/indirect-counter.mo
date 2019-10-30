let a = actor {
  let aa = actor {
    var c = 1;
    public func inc() {
      c += 1;
      printNat(c)
    };
    public func print() {
      printNat(c)
    };
  };
  public func inc() { aa.inc() };
  public func print() { aa.print() };
};

a.inc();
a.inc();
a.inc();
a.print()
