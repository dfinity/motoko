actor a {
  public func go() = ignore async {
    let o = object {
      public var x = await { async { 1 } };
      let a = debugPrintNat(x);
      // private b = (x := await { async (x + 1) });
      let b = (x := x + 1);
      let c = debugPrintNat(x);
      public func foo() = { x := x + 1 };
      let e = foo();
      let f = debugPrintNat(x);
    };
    debugPrint("done creating\n");
    debugPrintNat(o.x);
    o.x := o.x + 1;
    debugPrintNat(o.x);
    o.foo();
    debugPrintNat(o.x);
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
