let a = actor {
  let aa = actor {
    var c = 1;
    public func inc() {
      c += 1;
      debug_print_Nat(c)
    };
    public func debug_print() {
      debug_print_Nat(c)
    };
  };
  public func inc() { aa.inc() };
  public func debug_print() { aa.debug_print() };
};

a.inc();
a.inc();
a.inc();
a.debug_print()
