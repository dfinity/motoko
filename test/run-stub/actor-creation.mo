debugPrint ("main actor creating a");
actor a {
  public func foo() {
    debugPrint ("a.foo() called");

    debugPrint ("a creating b");
    actor b {
      public func foo() {
      debugPrint ("b.foo() called");
    };
    debugPrint ("b created");
    };

    debugPrint ("a calling b.foo()");
    b.foo();
    debugPrint ("a.foo() done");
  };
  debugPrint ("a created");
};
debugPrint ("main actor calling a.foo()");
a.foo();
