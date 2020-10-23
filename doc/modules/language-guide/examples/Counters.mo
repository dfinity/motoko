actor class Counter(init : Nat) {
  var count : Nat = init;

  public func increment() : async () {
    count += 1;
  };

  public query func get() : async Nat {
    count
  };

  public func set(n: Nat) : async () {
    count := n;
  };
}
