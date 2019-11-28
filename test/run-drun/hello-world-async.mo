actor a {
  public func world() {
    debugPrint("World!");
  };
  public func go() {
    a.world();
    debugPrint("Hello ");
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
