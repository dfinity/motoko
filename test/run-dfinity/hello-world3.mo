let a = actor {
  public func hello() {
    debug_print("Hello ");
  };
  public func go() {
    hello();
    world();
  };
  public func world() {
    debug_print("World!\n");
  };
};

a.go()
