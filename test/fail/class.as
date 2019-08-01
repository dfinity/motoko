// Correct annotations
class B() : {x : Int} = {
  public let x : Nat = 0
};

type T<A, B> = {x : Int; f : A -> B};

class C<A>(y : Nat) : T<A, A> {
  public let x = y;
  public let z = 1;
  public func f(x : A) : A { x };
  public func g() {};
};

let c = C<Bool>(7);
let _ = c.x + c.z;
if (c.f(true)) { c.g() };


type A = actor {m : () -> ()};

actor class AA() : A {
  public func m() {};
};


// Incorrect annotations
class D1() : {x : Int} {};
class D2() : {x : Nat} {
  public let x = -3;
};

class D3() : actor {} = {};
actor class D4() : {} = {};
