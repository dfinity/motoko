// import Prim "mo:prim";

type Result<T, E> = {
  #ok : T;
  #err : E;
};

module VarArray {
  public func map<T, U>(_ : [var T], _ : (T) -> U) : [var U] = [var];
};

let va = [var 1, 2, 3];

// choose_invariant: ?Nat <: U <: Any, choosing ?Nat
// OK: seems reasonable
let _ = VarArray.map(va, func x = if (x % 2 == 0) ?x else null);

// choose_invariant: {#err : Int; #ok : Nat} <: U <: Any, choosing {#err : Int; #ok : Nat}
// OK: but...
let _ = VarArray.map(va, func x = if (x % 2 == 0) #ok(x) else #err(-x));

// choose_invariant: Result<Nat, Int> <: U <: Any, choosing Result<Nat, Int>
let _ = VarArray.map(va, func x : Result<Nat, Int> = if (x % 2 == 0) #ok(x) else #err(-x));
let _ = VarArray.map(va, func x = if (x % 2 == 0) #ok(x) else #err(-x) : Result<Nat, Int>);

// choose_invariant: {#ok : Int} <: U <: Any, choosing {#ok : Int}
// NOK: would most likely need to be annotated
let _ = VarArray.map(va, func x = if (x % 2 == 0) #ok(x) else #ok(-x));
// let _ : [var Result<Int, Text>] = VarArray.map(va, func x = if (x % 2 == 0) #ok(x) else #ok(-x));

// choose_invariant: {v : {#ok : Nat}; x : Nat; y : Int; z : Null} <: U <: Any, choosing {v : {#ok : Nat}; x : Nat; y : Int; z : Null}
// NOK: I'd exclude Null and {#ok : Nat}
let _ = VarArray.map(va, func x = { x; y = -x; z = null; v = #ok(x) })

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
