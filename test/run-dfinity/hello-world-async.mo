let a = actor {
  public func world() {
    Debug.print("World!\n");
  };
  public func go() {
    world();
    Debug.print("Hello ");
  };
};

a.go()
