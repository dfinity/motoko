let _ = async {
  let o = new {
    var x = await { async { 1 } };
    private a = printInt(x);
    // private b = (x := await { async (x + 1) });
    private b = (x := x + 1);
    private c = printInt(x);
    foo() = { x := x + 1 };
    private e = foo();
    private f = printInt(x);
  };
  print("\ndone creating\n");
  printInt(o.x);
  o.x := o.x + 1;
  printInt(o.x);
  o.foo();
  printInt(o.x);
  print("\n");
}
