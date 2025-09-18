mixin(initial : Nat) {
  var counter = initial;
  public func increment() : async () {
    counter += 1;
    return;
  };
};
