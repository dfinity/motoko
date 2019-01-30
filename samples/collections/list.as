/*
 This file represents a kind of "warm up" for creating more involved
 collections, such as hash tables (which use linked lists internally)
 and persistant maps, which will follow similar functional prog
 patterns.
*/

// TODO-Matthew: Look at SML Basis Library; Look at OCaml List library.
// Write:
//  - iterator objects, for use in 'for ... in ...' patterns
//  - standard list recursors: foldl, foldr, iter
//  - standard higher-order combinators: map, filter, etc.
//  - lists+pairs: zip, split, etc

// polymorphic linked lists
type List<T> = ?(T, List<T>);

// empty list
func nil<T>() : List<T> = 
  null;

// test for empty list
func isnil<T>(l : List<T>) : Bool {
  switch l {
    case null { true  };
    case _    { false };
  }
};

// aka "list cons"
func push<T>(x : T, l : List<T>) : List<T> = 
  ?(x, l);

// get head of list
func hd<T>(l : List<T>) : ?T = {
  switch l {
    case null { null };
    case (?(h, _)) { ?h };
  }
};

// get tail of list
func tl<T>(l : List<T>) : ?List<T> = {
  switch l {
    case null { null };
    case (?(_, t)) { ?t };
  }
};

// treat the list as a stack; combines 'hd' and (non-failing) 'tl' into one operation
func pop<T>(l : List<T>) : (?T, List<T>) = {
  switch l {
    case null { (null, null) };
    case (?(h, t)) { (?h, t) };
  }
};

// array-like list access, but in linear time
func nth<T>(l : List<T>, n : Nat) : ?T = {
  switch (n, tl<T>(l)) {
    case (0, _)    { hd<T>(l) };
    case (m, null) { null };
    case (m, ?tl)  { nth<T>(tl, n - 1) };
  }
};

//////////////////////////////////////////////////////////////////

// # Example usage

type X = Nat;
func opnat_eq(a : ?Nat, b : ?Nat) : Bool { 
  switch (a, b) {
    case (null, null) { true };
    case (?aaa, ?bbb) { aaa == bbb };
    case (_,    _   ) { false };
  }
};
func opnat_isnull(a : ?Nat) : Bool { 
  switch a {
    case (null) { true };
    case (?aaa) { false };
  }
};

// ## Construction
let l1 = nil<X>();
let l2 = push<X>(2, l1);
let l3 = push<X>(3, l2);

// ## Projection
assert (opnat_eq(nth<X>(l3, 0), ?3));
assert (opnat_eq(nth<X>(l3, 1), ?2));
assert (opnat_eq(nth<X>(l3, 2), null));
assert (opnat_eq (hd<X>(l3), ?3));
assert (opnat_eq (hd<X>(l2), ?2));
assert (opnat_isnull(hd<X>(l1)));

// ## Deconstruction
let (a1, t1) = pop<X>(l3);
assert (opnat_eq(a1, ?3));
let (a2, t2) = pop<X>(l2);
assert (opnat_eq(a2, ?2));
let (a3, t3) = pop<X>(l1);
assert (opnat_eq(a3, null));
assert (isnil<X>(t3));
