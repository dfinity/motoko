persistent actor {
  class Clamper(l : Nat, h : Nat) {
    public func clamp(x : Nat) : Nat {
      // When trying to evolve this class to hardcode the upper limit,
      // one needs to make sure to still close over every variable as before
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
