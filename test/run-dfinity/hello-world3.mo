let a = actor {
  public func hello() {
    Debug.print("Hello ");
  };
  public func go() {
    hello();
    world();
  };
  public func world() {
    Debug.print("World!\n");
  };
};

a.go()
