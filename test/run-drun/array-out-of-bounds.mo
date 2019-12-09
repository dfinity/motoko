actor a {
  let xs = [0, 1, 2, 3, 4];
  let ys = [];

  public func foo1() {
    ignore(xs[5]);
    debugPrint("Unreachable code reached");
  };
  public func foo2() {
    ignore(ys[0]);
    debugPrint("Unreachable code reached");
  };
};

a.foo1(); //OR-CALL ingress foo1 0x4449444C0000
a.foo2(); //OR-CALL ingress foo2 0x4449444C0000

