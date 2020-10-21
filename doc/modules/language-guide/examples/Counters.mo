actor class Counter(init : Nat) {
  var count : Nat = init;

  public func increment() : async () {
    count += 1;
  };

  public query func get_current() : async Nat {
    count
  };

  public func set_current(n: Nat) : async () {
    count := n;
  };
}
