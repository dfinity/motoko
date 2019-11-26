actor a {
  public func foo() { debugPrint("a"); };

  public func pass_func(f : shared () -> ()) {
    f();
  };

  public func pass_actor(a : actor { foo : shared () -> () }) {
    a.foo();
  };

  public func go() {
    actor b {
      public func foo() { debugPrint("b"); };
    };
    pass_func(foo);
    pass_func(b.foo);
    pass_actor(b);
  };

  go();
}
