actor class Counter(init : Nat) {

  var count : Nat = init;

  public func inc() : async () {
    count += 1;
  };

  public func get() : async Nat {
    count
  };

  public func set(n: Nat) : async () {
    count := n;
  };
}
