import Prim "mo:prim";

module A {
  public let foo = 5;
  public let bar = Prim.deprecate(42);
  public let f : Deprecated<Nat -> Nat> = Prim.deprecate(func(x : Nat) : Nat { x });
  public func g(x : Nat) : Deprecated<Nat> { Prim.deprecate x };
};

assert (A.foo == 5); // no warning

assert (A.bar == 42); // with warning
ignore(A.bar); // no warning

ignore(A.f); // no warning
ignore(A.f(5)); // warning
assert(A.f(5) == 5); // warning

ignore(A.g); // no warning
ignore(A.g(5)); // no warning
assert(A.g(5) == 5); // warning
