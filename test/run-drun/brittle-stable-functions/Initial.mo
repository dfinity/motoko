persistent actor {
  class Clamper(l : Nat, h : Nat) {
    public func clamp(x : Nat) : Nat {
      if (x < l) l
      else if (x > h) h
      else x;
    };
  };

  let clamper : Clamper = Clamper(0, 5);

  public query func clamp10() : async Nat {
    return clamper.clamp(10);
  };
}
