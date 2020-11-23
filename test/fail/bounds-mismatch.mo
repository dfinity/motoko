{
  func f(g : <A>() -> ()) {};
  f(func<A>() {});  // okay
};

{
  func f(g : <A <: Nat>() -> ()) {};
  f(func<A <: Nat>() {});  // okay
};

{
  func f(g : <A>() -> ()) {};
  f(func() {});  // error
};

{
  func f(g : () -> ()) {};
  f(func<A>() {});  // error
};

{
  func f(g : <A>() -> ()) {};
  f(func<A <: Nat>() {});  // error
};

{
  func f(g : <A <: Nat>() -> ()) {};
  f(func<A>() {});  // error
};

{
  func f(g : <A <: Int>() -> ()) {};
  f(func<A <: Nat>() {});  // error
};

{
  func f(g : <A <: Nat>() -> ()) {};
  f(func<A <: Int>() {});  // error
};

{
  let _ = [func<A>() {}, func<A>() {}];  // okay
};

{
  let _ = [func<A <: Nat>() {}, func<A <: Nat>() {}];  // okay
};

{
  let _ = [func() {}, func<A>() {}];  // warn
};

{
  let _ = [func<A>() {}, func<A <: Nat>() {}];  // warn
};

{
  let _ = [func<A <: Int>() {}, func<A <: Nat>() {}];  // warn
};

{
  // okay
  let _ = [func(_ : <A <: Nat>() -> ()) {}, func(_ : <A <: Nat>() -> ()) {}];
};

{
  // no warning
  let _ : [None -> ()] = [func(_ : <A <: Nat>() -> ()) {}, func(_ : <A <: Int>() -> ()) {}];
};


{
type A = <X> X -> X;
type B = <Y <: Nat> Y -> Y;
func f(x : A) : B = x;
};

{
type A = <X <: Int, Y <: {}> X -> Y;
type B = <X <: Int, Y <: {x : Nat}> X -> Y;
func f(x : A) : B = x;
};

{
type A = <X <: Nat, Y> X -> Y;
type B = <Y <: Nat, X <: Y> Y -> X;
func f(x : A) : B = x;
};

{
type A = <X, Y> X -> X;
type B = <X <: Y, Y> X -> X;
func f(x : A) : B = x;
};

{
type A = <X, Y, Z <: X> X -> X;
type B = <X, Y <: X, Z <: Y> X -> X;
func f(x : A) : B = x;
};

{
class C() { public let x = 0 };
type A = <X <: {x : Int}> X -> X;
type B = <X <: C> X -> X;
func f(x : A) : B = x;
};

{
class C<X <: Int>() {public func f() : X { f() }};
type A = <X, Y <: {f : () -> X}> X -> X;
type B = <X <: Nat, Y <: C<X>> X -> X;
func f(x : A) : B = x;
};

{
class C<X <: Int>() {public func f() : X { f() }};
type A = <X, Y <: {f : () -> Nat}> X -> X;
type B = <X <: Nat, Y <: C<X>> X -> X;
func f(x : A) : B = x;
};


{
  let poly_funcs4 = [
    func<A <: Int, B <: Nat> (as : [A], b : B) : A = as[0],
    func<B <: Nat, A <: Int> (bs : [B], a : A) : B = bs[0]
  ];
};
