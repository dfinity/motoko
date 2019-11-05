let a = actor {
  public func hello() {
    debugPrint("Hello ");
  };
  public func go() {
    hello();
    world();
  };
  public func world() {
    debugPrint("World!\n");
  };
};

a.go()
