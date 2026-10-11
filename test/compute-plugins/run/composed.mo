//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx --plugin ../../../_build/compute-plugins/sha256
import Prim "mo:⛔";

let first = { amount = 255 : Nat; omitted = null : ?Text };
let second = { amount = 255 : Nat };
assert Prim.sha256(Prim.to_candid_canonical(first)) ==
  Prim.sha256(Prim.to_candid_canonical(second));
assert Prim.sha256(Prim.to_candid_canonical(1 : Nat)) !=
  Prim.sha256(Prim.to_candid_canonical(2 : Nat));
