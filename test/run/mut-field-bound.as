type Obj = {var x : Nat};
func foo<O <: Obj>(o : O) : Nat {
  o.x
};
let o : Obj = new {var x : Nat = 1};
assert(foo<{var x : Nat}>(o) == 1);
