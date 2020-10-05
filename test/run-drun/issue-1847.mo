actor class (step : Nat) {
  var c = 0;
  public func inc() {
    c += step;
  };
  public func get() : async Nat {
    return c;
  };
}
