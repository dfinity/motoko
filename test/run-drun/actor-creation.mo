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

a.foo(); //OR-CALL ingress foo "DIDL\x00\x00"

// certainly won’t work on drun
//SKIP comp
