let _ = async {
  let o = object {
    public var x = await { async { 1 } };
    let a = printInt(x);
    // private b = (x := await { async (x + 1) });
    let b = (x := x + 1);
    let c = printInt(x);
    public func foo() = { x := x + 1 };
    let e = foo();
    let f = printInt(x);
  };
  print("\ndone creating\n");
  printInt(o.x);
  o.x := o.x + 1;
  printInt(o.x);
  o.foo();
  printInt(o.x);
  print("\n");
}
