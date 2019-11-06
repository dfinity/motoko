let _ = future {
  let o = object {
    public var x = await { future { 1 } };
    let a = printNat(x);
    // private b = (x := await { future (x + 1) });
    let b = (x := x + 1);
    let c = printNat(x);
    public func foo() = { x := x + 1 };
    let e = foo();
    let f = printNat(x);
  };
  print("\ndone creating\n");
  printNat(o.x);
  o.x := o.x + 1;
  printNat(o.x);
  o.foo();
  printNat(o.x);
  print("\n");
}
