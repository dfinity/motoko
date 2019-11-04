let a = actor {
  public func world() {
    debug_print("World!\n");
  };
  public func go() {
    world();
    debug_print("Hello ");
  };
};

a.go()
