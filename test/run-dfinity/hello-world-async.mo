let a = actor {
  public func world() {
    print("World!\n");
  };
  public func go() {
    world();
    print("Hello ");
  };
};

a.go()
