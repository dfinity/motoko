actor a {
  public func go() {
    actor b {
      public func bad() {
        debugPrint ("b.bad() called (should not happen)");
      }
    };
    b.bad();
    assert false;
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
