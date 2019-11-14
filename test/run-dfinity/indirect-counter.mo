let a = actor {
  let aa = actor {
    var c = 1;
    public func inc() {
      c += 1;
      Debug.printNat(c)
    };
    public func Debug.print() {
      Debug.printNat(c)
    };
  };
  public func inc() { aa.inc() };
  public func Debug.print() { aa.Debug.print() };
};

a.inc();
a.inc();
a.inc();
a.Debug.print()
