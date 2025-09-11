persistent actor {
  class Clamper(low : Nat, high : Nat) {
    public func clamp(x : Nat) : Nat {
      if (x < low) low
      else if (x > high) high
      else x;
    };
  };

  let clamper : Clamper = Clamper(0, 5);

  public query func clamp10() : async Nat {
    return clamper.clamp(10);
  };
}
