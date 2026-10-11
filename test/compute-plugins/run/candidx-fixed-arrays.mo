//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx

import Prim "mo:⛔";

type Row = { _0_ : Nat16; _1_ : Nat8 };
assert Prim.to_candid_canonical([] : [Row]) ==
  "DIDL\01\6d\6f\01\00\00";
assert Prim.to_candid_canonical([{ _0_ = 1; _1_ = 2 }] : [Row]) ==
  "DIDL\02\6d\01\6c\02\00\7a\01\7b\01\00\01\01\00\02";

// Records containing only nulls consume visits but have no payload bytes.
assert Prim.to_candid_canonical([{ _0_ = null }, { _0_ = null }]) ==
  "DIDL\02\6d\01\6c\01\00\7f\01\00\02";

// Binary lengths vary independently inside a vector's fixed wire shape.
// Blob and [Nat8] must share the same type-table entry.
let binaries = [
  { _0_ = "a" : Blob; _1_ = [] : [Nat8] },
  { _0_ = "" : Blob; _1_ = [0, 255] : [Nat8] },
];
assert Prim.to_candid_canonical(binaries) ==
  Prim.to_candid_canonical([
    { _0_ = "a" : Blob; _1_ = "" : Blob },
    { _0_ = "" : Blob; _1_ = "\00\ff" : Blob },
  ]);

// Deduplicate record children by wire shape even when Motoko layouts differ.
let chars = [{ _0_ = { _0_ = 'a' }; _1_ = { _0_ = 97 : Nat32 } }];
assert Prim.to_candid_canonical(chars) ==
  "DIDL\03\6d\01\6c\02\00\02\01\02\6c\01\00\79\01\00\01\61\00\00\00\61\00\00\00";

// Type recursion must terminate in the fixed-size analysis even when there
// are no values of the recursive type in this particular input.
type Recursive = { next : Recursive };
assert Prim.to_candid_canonical([] : [Recursive]) ==
  "DIDL\01\6d\6f\01\00\00";
