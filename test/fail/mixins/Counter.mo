mixin(initial : Nat) {
  type T = Nat;
  var counter = initial;
  public func increment() : async () {
    counter += 1;
    return;
  };
};
