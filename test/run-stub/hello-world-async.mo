let a = actor {
  public func world() {
    debugPrint("World!");
  };
  public func go() {
    a.world();
    debugPrint("Hello ");
  };
};

a.go()

