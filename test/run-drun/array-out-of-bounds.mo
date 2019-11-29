actor a {
  let xs = [0, 1, 2, 3, 4];
  let ys = [];

  public func foo1() : async () {
    ignore(xs[5]);
    debugPrint("Unreachable code reached");
  };
  public func foo2() : async () {
    ignore(ys[0]);
    debugPrint("Unreachable code reached");
  };
};

//CALL ingress foo1 0x4449444C0000
//CALL ingress foo2 0x4449444C0000

