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

// get tail of list, as a list
func tl<T>(l : List<T>) : List<T> = {
  switch l {
  case null      { null };
  case (?(_, t)) { t };
  }
};

// get tail of list, as an optional list
func tlo<T>(l : List<T>) : ?List<T> = {
  switch l {
  case null      { null };
  case (?(_, t)) { ?t };
  }
};

// treat the list as a stack; combines 'hd' and (non-failing) 'tl' into one operation
func pop<T>(l : List<T>) : (?T, List<T>) = {
  switch l {
  case null      { (null, null) };
  case (?(h, t)) { (?h, t) };
  }
};

// length; tail recursive
func len<T>(l : List<T>) : Nat = {
  func rec(l : List<T>, n : Nat) : Nat {
    switch l {
      case null     { n };
      case (?(_,t)) { rec(t,n+1) };
    }
  };
  rec(l,0)
};

// array-like list access, but in linear time; tail recursive
func nth<T>(l : List<T>, n : Nat) : ?T = {
  switch (n, l) {
  case (0, _)     { hd<T>(l) };
  case (_, null)  { null };
  // XXX
  //case (_, ?cons) { nth<T>(tl<T>(cons), n - 1) };
  }
};

// array-like list access, but in linear time; tail recursive
func nth_<T>(l : List<T>, n : Nat) : ?T = {
  switch (n, tlo<T>(l)) {
  case (0, _)    { hd<T>(l) };
  case (_, null) { null };
  case (_, ?t)   { nth_<T>(t, n - 1) };
  }
};

// map; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive map)
func map<T,S>(l : List<T>, f:T -> S) : List<S> = {
  func rec(l : List<T>) : List<S> {
    switch l {
      case null     { null };
      case (?(h,t)) { ?(f(h),rec(t)) };
    }
  };
  rec(l)
};

// filter; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive version)
func filter<T>(l : List<T>, f:T -> Bool) : List<T> = {
  func rec(l : List<T>) : List<T> {
    switch l {
      case null     { null };
      case (?(h,t)) { if (f(h)){ ?(h,rec(t)) } else { rec(t) } };
    }
  };
  rec(l)
};

// map-and-filter; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive version)
func mapfilter<T,S>(l : List<T>, f:T -> ?S) : List<S> = {
  func rec(l : List<T>) : List<S> {
    switch l {
      case null     { null };
      case (?(h,t)) {
        switch (f(h)) {
        case null { rec(t) };
        case (?h_){ ?(h_,rec(t)) };
        }
      };
    }
  };
  rec(l)
};

// append; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive version)
func append<T>(l : List<T>, m : List<T>) : List<T> = {
  func rec(l : List<T>) : List<T> {
    switch l {
    case null     { m };
    case (?(h,t)) {?(h,rec(l))};
    }
  };
  rec(l)
};

// take; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive version)
func take<T>(l : List<T>, n:Nat) : List<T> = {
  switch (l, n) {
  case (_, 0) { null };
  case (null,_) { null };
  // XXX: Compiler bug?
  //case (?(h, t), m) {?(h, take<T>(t, m - 1))};
  }
};

// drop; tail recursive
func drop<T>(l : List<T>, n:Nat) : List<T> = {
  switch (l, n) {
  case (l_,     0) { l_ };
  case (null,   _) { null };
  // XXX: Compiler bug?
  //case ((?(h,t)), m) { drop<T>(t, m - 1) };
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

// ## Projection -- use nth_
assert (opnat_eq(nth_<X>(l3, 0), ?3));
assert (opnat_eq(nth_<X>(l3, 1), ?2));
assert (opnat_eq(nth_<X>(l3, 2), null));
assert (opnat_eq (hd<X>(l3), ?3));
assert (opnat_eq (hd<X>(l2), ?2));
assert (opnat_isnull(hd<X>(l1)));

/*
// ## Projection -- use nth
assert (opnat_eq(nth<X>(l3, 0), ?3));
assert (opnat_eq(nth<X>(l3, 1), ?2));
assert (opnat_eq(nth<X>(l3, 2), null));
assert (opnat_eq (hd<X>(l3), ?3));
assert (opnat_eq (hd<X>(l2), ?2));
assert (opnat_isnull(hd<X>(l1)));
*/

// ## Deconstruction
let (a1, t1) = pop<X>(l3);
assert (opnat_eq(a1, ?3));
let (a2, t2) = pop<X>(l2);
assert (opnat_eq(a2, ?2));
let (a3, t3) = pop<X>(l1);
assert (opnat_eq(a3, null));
assert (isnil<X>(t3));

// ## List functions
assert (len<X>(l1) == 0);
assert (len<X>(l2) == 1);
assert (len<X>(l3) == 2);


////////////////////////////////////////////////////////////////
// For comparison:
//
// SML Basis Library Interface
// http://sml-family.org/Basis/list.html
//
// datatype 'a list = nil | :: of 'a * 'a list
// exception Empty
//
// val null : 'a list -> bool
// val length : 'a list -> int
// val @ : 'a list * 'a list -> 'a list
// val hd : 'a list -> 'a
// val tl : 'a list -> 'a list
// val last : 'a list -> 'a
// val getItem : 'a list -> ('a * 'a list) option
// val nth : 'a list * int -> 'a
// val take : 'a list * int -> 'a list
// val drop : 'a list * int -> 'a list
// val rev : 'a list -> 'a list
// val concat : 'a list list -> 'a list
// val revAppend : 'a list * 'a list -> 'a list
// val app : ('a -> unit) -> 'a list -> unit
// val map : ('a -> 'b) -> 'a list -> 'b list
// val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
// val find : ('a -> bool) -> 'a list -> 'a option
// val filter : ('a -> bool) -> 'a list -> 'a list
// val partition : ('a -> bool)
//                   -> 'a list -> 'a list * 'a list
// val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
// val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
// val exists : ('a -> bool) -> 'a list -> bool
// val all : ('a -> bool) -> 'a list -> bool
// val tabulate : int * (int -> 'a) -> 'a list
// val collate : ('a * 'a -> order)
//                 -> 'a list * 'a list -> order
//
////////////////////////////////////////////////////////////
