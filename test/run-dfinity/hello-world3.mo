let a = actor {
  public func hello() {
    print("Hello ");
  };
  public func go() {
    hello();
    world();
  };
  public func world() {
    print("World!\n");
  };
};

a.go()
