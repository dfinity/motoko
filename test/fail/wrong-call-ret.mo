
module M {
  public type M = { a : Float; var b : Text };
  public func empty() : M { { a = 0.0; var b = "" } };
  public func deleteMono(self : M, _ : Nat) : Bool { true };
  public func deletePoly<T>(self : M, _ : T) : Bool { true };
  public func mapMono(self : M, _ : Nat -> Nat) : M { self };
  public func mapPoly<T>(self : M, _ : T -> T) : M { self };
};

var target = M.empty();
target := M.deleteMono(target, 42);
target := M.deletePoly<Nat>(target, 42);
target := M.deletePoly(target, 42); // this should be the same error as above

let m1 : M.M = M.deleteMono(target, 42);
let m2 : M.M = M.deletePoly<Nat>(target, 42);
let m3 : M.M = M.deletePoly(target, 42); // ditto

M.deleteMono(target, 42);
M.deletePoly<Nat>(target, 42);
M.deletePoly(target, 42); // ditto

M.mapMono(target, func x = x + 1);
M.mapPoly<Nat>(target, func x = x + 1);
M.mapPoly(target, func x = x + 1); // ditto

ignore M.mapMono(M.deleteMono(target, 42), func x = x + 1);
ignore M.mapMono(M.deletePoly<Nat>(target, 42), func x = x + 1);
ignore M.mapMono(M.deletePoly(target, 42), func x = x + 1); // ditto
