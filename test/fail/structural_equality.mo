(func (x : Nat) : Nat = x) == (func (x : Nat) : Nat = x);

{ var x = 10 } == { var x = 10 };

class A() { public func inner() : Nat = 1 };
A() == A();

ignore ({ x = 10 } == { var x = 10 });

assert(10 == "hi");

func myEq<A>(x : A, y : A) : Bool = x == y;
func myEq2<A, B>(x : A, y : B) : Bool = x == y;

ignore (10 != "hi");
ignore ({ x = { p = 1 } } == { x = { q = 1 } });

// one operand already has the single-valued type, the other is a strict subtype
func isEmpty(config : { name : Text }) : Bool = config == {};
func anyEq(x : Any, y : Nat) : Bool = x == y;
func anyEqGeneric<A>(x : A, y : Any) : Bool = x == y;
func keyEq(k : { kind : { #key }; id : Nat }, j : { kind : { #key } }) : Bool = k == j;
