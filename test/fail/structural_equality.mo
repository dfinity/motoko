(func (x : Nat) : Nat = x) == (func (x : Nat) : Nat = x);

{ var x = 10 } == { var x = 10 };

class A() { public func inner() : Nat = 1 };
A() == A();

ignore ({ x = 10 } == { var x = 10 });
let r1 = { a = "abc"; x = 10 };
let r2 = { a = "def"; var x = 10 };
ignore (r1 == r2);
ignore ((r1 : { a : Text }) == r2); // explicit annotation to suppress the message

assert(10 == "hi");

func myEq<A>(x : A, y : A) : Bool = x == y;
func myEq2<A, B>(x : A, y : B) : Bool = x == y;
