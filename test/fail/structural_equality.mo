(func (x : Nat) : Nat = x) == (func (x : Nat) : Nat = x);

{ var .x = 10 } == { var .x = 10 };

class A() { public func inner() : Nat = 1 };
A() == A();

ignore ({ .x = 10 } == { var .x = 10 });

assert(10 == "hi");
