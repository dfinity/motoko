//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx
//MOC-FLAG --plugin ../../../_build/compute-plugins/sha256

import Prim "mo:⛔";

// Distinct Motoko types share a canonical wire shape. Header numbering must
// deduplicate the inner records by their wire types, including Char/Nat32.
let record = { _0_ = { _0_ = 'a' }; _1_ = { _0_ = 97 : Nat32 } };
assert Prim.to_candid_canonical(record) ==
  "DIDL\02\6c\02\00\01\01\01\6c\01\00\79\01\00\61\00\00\00\61\00\00\00";

// Blob and [Nat8] also share a wire shape, including when empty.
let binary = { _0_ = "abc" : Blob; _1_ = [] : [Nat8] };
assert Prim.to_candid_canonical(binary) ==
  "DIDL\02\6c\02\00\01\01\01\6d\7b\01\00\03abc\00";
assert Prim.to_candid_canonical("" : Blob) ==
  Prim.to_candid_canonical([] : [Nat8]);

// A primitive vector has a different empty wire type except for Nat8.
assert Prim.to_candid_canonical([] : [Nat64]) ==
  "DIDL\01\6d\6f\01\00\00";
assert Prim.to_candid_canonical([1, 2] : [Nat64]) ==
  "DIDL\01\6d\78\01\00\02\01\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00";
assert Prim.to_candid_canonical([null, null] : [Null]) ==
  "DIDL\01\6d\7f\01\00\02";
assert Prim.to_candid_canonical([42, "ignored"] : [Any]) ==
  "DIDL\01\6d\70\01\00\02";
let texts = ["", "hello", "λ🌐"];
assert Prim.to_candid_canonical(texts) ==
  "DIDL\01\6d\71\01\00\03\00\05hello\06\ce\bb\f0\9f\8c\90";

// The large-variant dispatcher uses Motoko heap hashes for selection and
// Candid label IDs for encoding. Numeric labels deliberately differ.
type Choice = {
  #alpha : Nat; #beta : Nat; #done : Nat; #ready : Nat; #zulu : Nat;
  #_0_ : Nat; #_1_ : Nat; #_4294967295_ : Nat;
};
func wide(value : Choice) : Blob { Prim.to_candid_canonical(value) };
assert wide(#alpha(1)) == Prim.to_candid_canonical(#alpha(1 : Nat));
assert wide(#beta(2)) == Prim.to_candid_canonical(#beta(2 : Nat));
assert wide(#done(3)) == Prim.to_candid_canonical(#done(3 : Nat));
assert wide(#ready(4)) == Prim.to_candid_canonical(#ready(4 : Nat));
assert wide(#zulu(5)) == Prim.to_candid_canonical(#zulu(5 : Nat));
assert wide(#_0_(6)) == Prim.to_candid_canonical(#_0_(6 : Nat));
assert wide(#_1_(7)) == Prim.to_candid_canonical(#_1_(7 : Nat));
assert wide(#_4294967295_(8)) ==
  Prim.to_candid_canonical(#_4294967295_(8 : Nat));
let choices : [Choice] = [#zulu(4), #_4294967295_(3), #ready(2), #_0_(1),
  #alpha(0), #_1_(8), #beta(7), #done(6), #done(5)];
Prim.debugPrint(debug_show Prim.sha256(Prim.to_candid_canonical(choices)));

// Balanced joins must preserve nested optional-field promotion and must keep
// independent accumulator state while deriving nested arrays.
type Row = { a : ??Nat; b : ?Blob };
let rows : [[Row]] = [
  [{ a = null; b = ?"a" }, { a = ?null; b = null }],
  [], [{ a = ??17; b = ?"" }],
  [{ a = ??42; b = null }, { a = null; b = null }, { a = ?null; b = ?"b" }],
];
Prim.debugPrint(debug_show Prim.sha256(Prim.to_candid_canonical(rows)));
