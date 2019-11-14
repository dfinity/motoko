let _ = async {
  let o = object {
    public var x = await { async { 1 } };
    let a = Debug.printNat(x);
    // private b = (x := await { async (x + 1) });
    let b = (x := x + 1);
    let c = Debug.printNat(x);
    public func foo() = { x := x + 1 };
    let e = foo();
    let f = Debug.printNat(x);
  };
  Debug.print("\ndone creating\n");
  Debug.printNat(o.x);
  o.x := o.x + 1;
  Debug.printNat(o.x);
  o.foo();
  Debug.printNat(o.x);
  Debug.print("\n");
}
