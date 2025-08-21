import Prim "mo:prim";

module VarArray {
  public func map<T, U>(_ : [var T], _ : (T) -> U) : [var U] = [var];
};

let va = [var 1, 2, 3];

// Test that we can solve invariant U when the bound is an 'isolated' type, e.g.
// Trying to solve:  Bool  <:  U  <:  Any
// Should pick U = Bool, because Bool has no proper supertypes/subtypes (except Any and Non)
let _ = VarArray.map(va, func x = x % 2 == 0); // Bool
let _ = VarArray.map(va, func x = Prim.natToNat8(x) + 1); // NatX
let _ = VarArray.map(va, func x = Prim.nat64ToInt64(Prim.natToNat64(x)) - 1); // IntX
let _ = VarArray.map(va, func x = x % 2 == 0); // Float
let _ = VarArray.map(va, func x = Prim.nat32ToChar(Prim.natToNat32(x))); // Char
let _ = VarArray.map(va, func x = debug_show (x)); // Text
let _ = VarArray.map(va, func x = Prim.encodeUtf8(debug_show (x))); // Blob
let _ = VarArray.map(va, func x = Prim.principalOfBlob(Prim.encodeUtf8(debug_show (x)))); // Principal
let _ = VarArray.map(va, func x = [Prim.natToNat16(x)]); // [Nat16]
let _ = VarArray.map(va, func x = [var x]); // [var Nat]
let _ = VarArray.map(va, func x = (Prim.intToInt32(x), debug_show (x))); // (Int32, Text)
let _ = VarArray.map(va, func x = func(y : Nat8) : Bool = Prim.natToNat8(x) == y); // Nat8 -> Bool

// Counter examples that should fail:
func _m1() {
  let _ = VarArray.map(va, func x = x); // Nat
};
func _m2() {
  let _ = VarArray.map(va, func x = x : Int); // Int
};
func _m3() {
  let _ = VarArray.map(va, func x = null); // Null
};
func _m4() {
  let _ = VarArray.map(va, func x = ?Prim.natToNat8(x)); // ?Nat8
};
func _m5() {
  let _ = VarArray.map(va, func x = [x]); // [Nat]
};
func _m6() {
  let _ = VarArray.map(va, func x = { x }); // { x : Nat }
};
func _m7() {
  let _ = VarArray.map(va, func x = #c(x)); // { #c : Nat }
};

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
