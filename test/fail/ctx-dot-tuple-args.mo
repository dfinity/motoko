module M {
  public type T = (Nat, Nat);
  public let i : Nat = 42;
  public func f1(self : T, _x : Nat, _y : Nat) {};
  public func f2(self : T, _xy : (Nat, Nat)) {};
  public func f3(self : T, _i : (implicit : (i : Nat))) {};
  public func f4(self : T, _xy : (Nat, Nat), _i : (implicit : (i : Nat))) {};
};

let s : M.T = (0, 0);
func m1() {
  s.f1(10, 20); // ok
  s.f1((10, 20)); // er: 1 arg provided, 2 expected
  s.f1(s); // er: 1 arg provided, 2 expected

  s.f2(10, 20); // er: 2 args provided, 1 tuple expected
  s.f2((10, 20)); // ok
  s.f2(s); // ok

  s.f3(); // ok: implicit resolved
};
func m2() {
  s.f4(10, 20); // er: 2 args provided, no implicit inserted, Nat </: (Nat, Nat)
};
func m3() {
  s.f4((10, 20)); // ok: 1 arg, 1 implicit inserted
  s.f4(s); // ok: 1 arg, 1 implicit inserted
};
