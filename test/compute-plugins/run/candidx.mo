//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx
import Prim "mo:⛔";

// Independent wire-format examples, including zero arguments and one tuple.
assert Prim.to_candid_canonical() == "DIDL\00\00";
assert Prim.to_candid_canonical(()) == "DIDL\00\01\7f";
assert Prim.to_candid_canonical(255 : Nat) == "DIDL\00\01\7d\ff\01";
assert Prim.to_candid_canonical(-65 : Int) == "DIDL\00\01\7c\bf\7f";
assert Prim.to_candid_canonical(1 : Nat, true) ==
  "DIDL\00\02\7d\7e\01\01";
assert Prim.to_candid_canonical((1 : Nat, true)) ==
  "DIDL\01\6c\02\00\7d\01\7e\01\00\01\01";
let pair = (1 : Nat, true);
assert Prim.to_candid_canonical(pair) == Prim.to_candid_canonical((1 : Nat, true));

// Observe a cold native BigInt literal before other arithmetic allocates it.
let large = 18446744073709551616 : Nat;
assert Prim.to_candid_canonical(large) ==
  "DIDL\00\01\7d\80\80\80\80\80\80\80\80\80\02";
let missing : { amount : Nat; missing : ?Text } = { amount = 255; missing = null };
assert Prim.to_candid_canonical(missing) ==
  "DIDL\01\6c\01\d8\a3\8c\a8\0d\7d\01\00\ff\01";
assert Prim.to_candid_canonical(missing) ==
  Prim.to_candid_canonical({ amount = 255 : Nat });

assert Prim.to_candid_canonical("" : Blob) ==
  "DIDL\01\6d\7b\01\00\00";
assert Prim.to_candid_canonical("\00\ff" : Blob) ==
  Prim.to_candid_canonical([0, 255] : [Nat8]);
assert Prim.to_candid_canonical([] : [Nat]) ==
  "DIDL\01\6d\6f\01\00\00";
assert Prim.to_candid_canonical([null, ?128] : [?Nat]) ==
  "DIDL\02\6d\01\6e\7d\01\00\02\00\01\80\01";
assert Prim.to_candid_canonical([?(?null), null] : [??Null]) ==
  "DIDL\03\6d\01\6e\02\6e\7f\01\00\02\01\01\00";
assert Prim.to_candid_canonical({ _4294967295_ = 1 : Nat }) ==
  "DIDL\01\6c\01\ff\ff\ff\ff\0f\7d\01\00\01";
assert Prim.to_candid_canonical(#_4294967295_) ==
  "DIDL\01\6b\01\ff\ff\ff\ff\0f\7f\01\00\00";
assert Prim.to_candid_canonical("\u{FEFF}λ") ==
  "DIDL\00\01\71\05\ef\bb\bf\ce\bb";

// Inferred aliases keep the intrinsic identity across records and generics.
let encode = Prim.to_candid_canonical;
let operations = { encode };
func identity<T>(value : T) : T = value;
let genericAlias = identity(encode);
assert operations.encode(42 : Nat) == genericAlias(42 : Nat);
module Encoders {
  public let canonical = Prim.to_candid_canonical;
  public func encodeNat(value : Nat) : Blob = canonical(value);
};
assert Encoders.canonical(42 : Nat) == Encoders.encodeNat(42);
let encoders = [encode, Encoders.canonical];
assert encoders[1](42 : Nat) == genericAlias(42 : Nat);
let optionalEncoder = ?encode;
switch optionalEncoder {
  case (?f) { assert f(42 : Nat) == Encoders.canonical(42 : Nat) };
  case null { assert false };
};
assert encode(true, 42 : Nat) == Prim.to_candid_canonical(true, 42 : Nat);
assert encode((true, 42 : Nat)) == Prim.to_candid_canonical((true, 42 : Nat));

// Specializing the call must preserve evaluation of the callee and arguments.
var effects = "";
let ordered = (do { effects #= "callee;"; encode })(
  do { effects #= "first;"; 1 : Nat },
  do { effects #= "second;"; true },
);
assert effects == "callee;first;second;";
assert ordered == Prim.to_candid_canonical(1 : Nat, true);
