//MOC-FLAG -W M0238
let n = 1;

func foo<T>(x : T, f : (T, T) -> T) : T = f(x, x);

ignore foo(n, func (x, y) = x + y); // no warning

// With Warnings
ignore foo(n, func (x : Nat, y : Nat) : Nat = x + y);
ignore foo(n, func (x, y) : Nat = x + y);
ignore foo(n, func (x : Nat, y) = x + y);
ignore foo(n, func (x, y : Nat) = x + y);
