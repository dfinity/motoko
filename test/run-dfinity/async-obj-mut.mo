let _ = async {
  let o = object {
    public var x = await { async { 1 } };
    let a = debug_print_Nat(x);
    // private b = (x := await { async (x + 1) });
    let b = (x := x + 1);
    let c = debug_print_Nat(x);
    public func foo() = { x := x + 1 };
    let e = foo();
    let f = debug_print_Nat(x);
  };
  debug_print("\ndone creating\n");
  debug_print_Nat(o.x);
  o.x := o.x + 1;
  debug_print_Nat(o.x);
  o.foo();
  debug_print_Nat(o.x);
  debug_print("\n");
}
