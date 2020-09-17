// A simple Counter actor.

actor class Counter(i : Int) {
  flexible var c = i;
  func show(note : Text, c : Int) {};
  // Decrement counter
  public func dec() {
    show("dec", c);
    c -= 1;
  };

  // Read counter, asynchronous
  public func read() : async Int { c };
};

