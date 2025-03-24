import Prim "mo:⛔";

// @verify
actor OddEven {
  func odd(n: Nat) : Nat {
    assert:func (n % 2 == 1);
    assert:system Prim.exists<Nat>(func (k : Nat) = 2 * k + 1 == n);
    return n;
  };

  func even(n: Nat) : Nat {
    assert:func (n % 2 == 0);
    assert:system Prim.exists<Nat>(func (k : Nat) = 2 * k == n);
    return n;
  };
}
