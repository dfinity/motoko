persistent actor {
  class Clamper(l : Nat, h : Nat) {
    public func clamp(x : Nat) : Nat {
      ignore h;
      if (x < l) l
      else if (x > 10) 10
      else x;
    };
  };

  let clamper : Clamper = Clamper(0, 10);

  public query func clamp10() : async Nat {
    return clamper.clamp(10);
  };
}
