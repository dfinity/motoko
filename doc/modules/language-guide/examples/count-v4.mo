import Int "mo:base/Int";

actor Counter_v4 {

  stable var value : Int = 0;
  stable var newValue : Nat = Int.abs(value);

  public func inc() : async Nat {
    newValue += 1;
    return newValue;
  };

  public query func read() : async Nat {
    return newValue;
  }
}
