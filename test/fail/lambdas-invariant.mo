import Prim "mo:prim";

type Result<T, E> = {
  #ok : T;
  #err : E;
};

module VarArray {
  public func map<T, U>(_ : [var T], _ : (T) -> U) : [var U] = [var];
};

let va = [var 1, 2, 3];

let _ = VarArray.map(va, func x = x % 2 == 0); // Bool
let _ = VarArray.map(va, func x = Prim.natToNat8(x) + 1); // NatX
let _ = VarArray.map(va, func x = Prim.nat64ToInt64(Prim.natToNat64(x)) - 1); // IntX
let _ = VarArray.map(va, func x = x % 2 == 0); // Float
// Char
// Text
// Blob
// Principal
// [Nat16]
// [var Nat]
// (Int32, Text)

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
