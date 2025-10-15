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

type A = {};
type B = { a : Int };
type C = { a : Int; b : Text };

let a : A = {};
let b : B = { a = 0 };
let c : C = { a = 0; b = "" };

module MA {
  public type Self = A;
  public func cmp(_ : A, _ : A) : Nat = 1;
};

module MB {
  public type Self = B;
  public func cmp(_ : B, _ : B) : Nat = 2;
};

module MC {
  public type Self = C;
  public func cmp(_ : C, _ : C) : Nat = 3;
};

assert a.cmp(b) == 1;
assert b.cmp(c) == 2;
assert c.cmp(c) == 3;
