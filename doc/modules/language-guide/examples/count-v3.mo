actor Counter_v3 {

  stable var value : Nat = 0;

  public func inc() : async Nat {
    value += 1;
    return value;
  };

  public query func read() : async Nat { return value; }
}
