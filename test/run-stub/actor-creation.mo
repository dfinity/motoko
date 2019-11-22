debugPrint ("main actor creating a\n");
actor a {
  public func foo() {
    debugPrint ("a.foo() called\n");

    debugPrint ("a creating b\n");
    actor b {
      public func foo() {
      debugPrint ("b.foo() called\n");
    };
    debugPrint ("b created\n");
    };

    debugPrint ("a calling b.foo()\n");
    b.foo();
    debugPrint ("a.foo() done\n");
  };
  debugPrint ("a created\n");
};
debugPrint ("main actor calling a.foo()\n");
a.foo();
