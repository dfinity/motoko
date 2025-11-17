import Prim "mo:prim";

type C<T> = T -> ();
func c<T>(_ : T) : C<T> = func _ = ();

module VarArray {
  public func map<T, U>(_ : [var T], _ : (T) -> U) : [var U] = [var];
  public func map2<T, U1, U2>(_ : [var T], _ : (T) -> (U1, U2)) : [var (U1, U2)] = [var];
  public func mapC<T, U>(_ : [var T], _ : (T) -> C<U>) : [var U] = [var];
  public func invariantFirstRound<T, U>(_ : T -> (), _ : T -> U) : [var T] = [var];
};

let va = [var 1, 2, 3];

// Tests like: Trying to solve:  Bool  <:  U  <:  Any
// Should pick U = Bool, because Bool has no proper supertypes (except Any)
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

// Tests like: Trying to solve:  Non  <:  U  <:  Nat
// Should pick U = Non, because Nat has no proper subtypes (except Non)
let _ = VarArray.mapC(va, func x = c<Bool>(x % 2 == 0)); // Bool -> ()
let _ = VarArray.mapC(va, func x = c<Nat>(x)); // Nat -> ()
func _cInt() {
  let _ = VarArray.mapC(va, func x = c<Int>(x)); // Int -> ()
};

// Int and Opt have no supertypes
let _ = VarArray.map(va, func x = x : Int); // Int
let _ = VarArray.map(va, func x = ?x : ?Int); // ?Int

// Null and Nat have no subtypes
let _ = VarArray.map(va, func x = func(_ : Nat) {}); // Nat -> ()
let _ = VarArray.map(va, func x = func(_ : Null) {}); // Null -> ()

// Mix
let _ = VarArray.map(va, func x = func(n : Nat, _ : Null) : (Int, ?Int, ?[var Nat]) = (n, ?n, ?[var n])); // (Nat, Null) -> (Int, ?Int, ?[var Nat])

// Counter examples that should fail:
func _m1() {
  let _ = VarArray.map(va, func x = x); // Nat
};
func _m3() {
  let _ = VarArray.map(va, func x = null); // Null
};
func _m4() {
  let _ = VarArray.map(va, func x = ?x); // ?Nat
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

// Termination check: should succeed
type Bot = [Bot];
let _ = VarArray.map(va, func x = [] : [Bot]); // [Bot]

// Type variables: isolated when unbounded
func tvUnbounded<Ok>(t : Ok) {
  let _ = VarArray.map(va, func _ = t); // Ok
};

func tvBoundedToIsolated<Er <: Text>(t : Er) {
  let _ = VarArray.map(va, func _ = t); // Er, should still fail because [var Er] =/= [var Text]
};

func tvBoundedToNat<Er <: Nat>(t : Er) {
  let _ = VarArray.map(va, func _ = t); // Er, should fail
};

func reportBothInvariantErrors() {
  let _ = VarArray.map2(va, func x = (x, null)); // (Nat, Null)
};

type User = {
  var name : Text;
  var age : Nat;
};

module ComplexTypes {
  let user : User = { var name = "John"; var age = 30 };
  let users : [User] = [user];
  func failing1() {
    let _ = VarArray.map(va, func x = user);
  };
  func failing2() {
    let _ = VarArray.map(va, func x = users);
  };
};

func invariantFirstRound() {
  func onNat(x : Nat) : () = ();
  func onInt(x : Int) : () = ();
  let _ = VarArray.invariantFirstRound(onNat, func x = x); // works
  let _ = VarArray.invariantFirstRound(onInt, func x = x); // fails when solving `T`, before solving `U`
}

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
