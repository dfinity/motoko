let a = actor {
  public func world() {
    debugPrint("World!\n");
  };
  public func go() {
    world();
    debugPrint("Hello ");
  };
};

a.go()
