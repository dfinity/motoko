module Int {
  public type Self = Int;
  public func eq(self : Int, other : Int) : Bool { assert false; self == other };
};

module Nat {
  public type Self = Nat;
  public func eq(self : Nat, other : Nat) : Bool { self == other };
};

let s : Nat = 41;
assert s.eq(s); // Disambiguates to Nat, because Nat <: Int
