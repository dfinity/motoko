let L = import "list.as";

type X = Nat;

func opnatEq(a : ?Nat, b : ?Nat) : Bool {
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
let l1 = L.nil<X>();
let l2 = L.push<X>(2, l1);
let l3 = L.push<X>(3, l2);

// ## Projection -- use nth
assert (opnatEq(L.nth<X>(l3, 0), ?3));
assert (opnatEq(L.nth<X>(l3, 1), ?2));
assert (opnatEq(L.nth<X>(l3, 2), null));
//assert (opnatEq (hd<X>(l3), ?3));
//assert (opnatEq (hd<X>(l2), ?2));
//assert (opnat_isnull(hd<X>(l1)));

/*
 // ## Projection -- use nth
 assert (opnatEq(nth<X>(l3, 0), ?3));
 assert (opnatEq(nth<X>(l3, 1), ?2));
 assert (opnatEq(nth<X>(l3, 2), null));
 assert (opnatEq (hd<X>(l3), ?3));
 assert (opnatEq (hd<X>(l2), ?2));
 assert (opnat_isnull(hd<X>(l1)));
 */

// ## Deconstruction
let (a1, t1) = L.pop<X>(l3);
assert (opnatEq(a1, ?3));
let (a2, t2) = L.pop<X>(l2);
assert (opnatEq(a2, ?2));
let (a3, t3) = L.pop<X>(l1);
assert (opnatEq(a3, null));
assert (L.isNil<X>(t3));

// ## L functions
assert (L.len<X>(l1) == 0);
assert (L.len<X>(l2) == 1);
assert (L.len<X>(l3) == 2);

// ## L functions
assert (L.len<X>(l1) == 0);
assert (L.len<X>(l2) == 1);
assert (L.len<X>(l3) == 2);
