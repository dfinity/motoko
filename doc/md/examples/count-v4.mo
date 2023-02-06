import Int "mo:base/Int";

actor Counter_v4 {

  stable var state : Int = 0;
  stable var newState : Nat = Int.abs(state);

  public func inc() : async Nat {
    newState += 1;
    return newState;
  };

  public query func read() : async Nat { return newState; }
}
