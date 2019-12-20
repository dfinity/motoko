//MOC-FLAG  --actor-idl idl-mo

import imported "ic:00";

type node = {head : Nat; tail : list};
type list = ?node;
type o = ?o;
type stream = ?{head : Nat; next : shared query () -> async stream};  
type tree = {#branch : {left : tree; right : tree; val : Int}; #leaf : Int};  
type s = actor {f : t; g : shared list -> async (tree, stream)};
type t = shared s -> async ();

type expected = actor {
  //node: node;
  _2669435454_ : shared () -> async stream;
  field : shared {_1291438163_ : Nat8; test : imported.tree} -> async {};
  fieldnat : shared {_2_ : Int; _50_ : Nat} -> async {_0_ : Int};
  o : shared imported.o -> async imported.o;
  oneway : shared Nat8 -> ();
  query_ : shared query imported.list -> async imported.tree;
  service : imported.t;
  tuple : shared ((Int, [Nat8], Text)) -> async {_0_ : Int; _1_ : Nat8};
  variant : shared {#A; #B; #C; #D : Float} -> async ()
};

let a: expected = imported;

//let a = [var imported];

//ignore (a : [var expected]); // variable arrays are invariant, so this checks type equality.

//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP drun-run
//SKIP wasm-run
