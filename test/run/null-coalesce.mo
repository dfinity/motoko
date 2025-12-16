import Prim "mo:prim";

let n1 = null;
let n2 : ?Int = null;
let nat = ?5;
let nn0 = null;
let nn1 = ?null;
let nn2 : ??Nat = ?null;
let nn3 = ??1;
let nn4 : ??Nat = ??1;

let t1 = n1 ?? 42;
assert (t1 == 42);
let t2 = n2 ?? 42;
assert (t2 == 42);

let u1 = nat ?? Prim.trap("");
assert (u1 == 5);

let w1 = nn1 ?? ?7;
assert (w1 == null);
let w2 = nn2 ?? ?7;
assert (w2 == null);
let w3 = nn3 ?? ?7;
assert (w3 == ?1);
let w4 = nn4 ?? ?7;
assert (w4 == ?1);

let q1 = nn0 ?? n2 ?? 42;
assert (q1 == 42);

module WithDo {
  func f(n : Nat) : ?Int { ?(n + 1) };
  public func app(m : Nat) : Int {
    (do ? {
      let y = f(m)!;
      let z = f(m + 1)!;
      y + z
    }) ?? 0;
  }
};

assert (WithDo.app(1) == 5);

// Blocks
let b1 = (do { // block is not allowed on LHS
  let x = 1;
  ?{x};
}) ?? {{x=0}}; // block is allowed on RHS, so the record needs extra braces
assert (b1 == {x=1});
let br = ?{x=1};
let b2 = br ?? {
  let x = 2;
  {x}
};
assert (b2 == {x=1});
