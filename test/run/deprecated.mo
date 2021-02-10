import Prim "mo:prim";

let x = Prim.deprecate(32);

assert (x == 32); // warning

module A {
  public let foo = 5;
  public let bar = Prim.deprecate(42);
  public let f : Deprecated<Nat -> Nat> = Prim.deprecate(func(x : Nat) : Nat { x });
  public func g(x : Nat) : Deprecated<Nat> { Prim.deprecate x };
};

assert (A.foo == 5); // no warning

assert (A.bar == 42); // warning
ignore(A.bar); // warning

ignore(A.f); // warning
ignore(A.f(5)); // warning
assert(A.f(5) == 5); // warning

ignore(A.g); // no warning
ignore(A.g(5)); // no warning
assert(A.g(5) == 5); // no warning?

