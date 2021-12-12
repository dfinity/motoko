actor Counter_v3 {

  stable var state : Nat = 0;

  public func inc() : async Nat {
    state += 1;
    return state;
  };

  public query func read() : async Nat { return state; }
}
