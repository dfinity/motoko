import Prim "mo:prim";

// TODO: Null type
// let n1 = null;
let n2 : ?Nat = null;
let nat = ?5;
// let nn1 = ?null;
let nn2 : ??Nat = ?null;
let nn3 = ??1;
let nn4 : ??Nat = ??1;

// let t1 = n1 ?? 42;
// assert (t1 == 42);
let t2 = n2 ?? 42;
assert (t2 == 42);

let u1 = nat ?? Prim.trap("");
assert (u1 == 5);

// let w1 = nn1 ?? ?7;
// assert (w1 == null);
let w2 = nn2 ?? ?7;
assert (w2 == null);
let w3 = nn3 ?? ?7;
assert (w3 == ?1);
let w4 = nn4 ?? ?7;
assert (w4 == ?1);
