//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx

import Prim "mo:⛔";

// Primitive cases have different wire opcodes. Collection must preserve
// each opcode, unsigned label ordering, and the original payload order.
type Choice = {
  #_0_ : Nat; #_1_ : Bool; #_2_ : Text;
  #_3_ : Nat8; #_4_ : Char; #_4294967295_;
};
let values : [Choice] = [
  #_2_("x"), #_0_(128), #_1_(true), #_4294967295_,
  #_3_(255), #_4_('a'), #_0_(0), #_0_(1),
];
assert Prim.to_candid_canonical(values) ==
  "DIDL\02\6d\01\6b\06\00\7d\01\7e\02\71\03\7b\04\79\ff\ff\ff\ff\0f\7f\01\00\08\02\01x\00\80\01\01\01\05\03\ff\04\61\00\00\00\00\00\00\01";

// Only observed cases belong in the wire type, including repeated tags.
assert Prim.to_candid_canonical([#_0_(1), #_0_(2)] : [Choice]) ==
  "DIDL\02\6d\01\6b\01\00\7d\01\00\02\00\01\00\02";
assert Prim.to_candid_canonical([] : [Choice]) ==
  "DIDL\01\6d\6f\01\00\00";

// Nested collectors must leave the enclosing record builder intact. These
// arrays derive independent subsets before their outer vector joins them.
let nested = [
  { _0_ = [#_0_(1)] : [Choice]; _1_ = 5 : Nat8 },
  { _0_ = [#_1_(true)] : [Choice]; _1_ = 6 : Nat8 },
];
assert Prim.to_candid_canonical(nested) ==
  "DIDL\04\6d\01\6c\02\00\02\01\7b\6d\03\6b\02\00\7d\01\7e\01\00\02\01\00\01\05\01\01\01\06";
