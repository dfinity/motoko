let a = actor {
  public func world() {
    debugPrint("World!");
  };
  public func go() {
    a.world();
    debugPrint("Hello ");
  };
};

//CALL ingress go 0x4449444C0000

