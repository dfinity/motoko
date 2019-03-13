/*
 * Lists, a la functional programming, in ActorScript.
 */

// Done:
//
//  - standard list definition
//  - standard list recursors: foldl, foldr, iter
//  - standard higher-order combinators: map, filter, etc.
//  - (Every function here: http://sml-family.org/Basis/list.html)

// TODO-Matthew: File issues:
//
//  - 'assert_unit' vs 'assert_any' (related note: 'any' vs 'none')
//  - apply type args, but no actual args? (should be ok, and zero cost, right?)
//  - unhelpful error message around conditional parens (search for XXX below)

// TODO-Matthew: Write:
//
//  - iterator objects, for use in 'for ... in ...' patterns
//  - lists+pairs: zip, split, etc
//  - regression tests for everything that is below


// polymorphic linked lists
type List<T> = ?(T, List<T>);

let List = new {

  // empty list
  func nil<T>() : List<T> =
    null;

  // test for empty list
  func isNil<T>(l : List<T>) : Bool {
    switch l {
    case null { true  };
    case _    { false };
    }
  };

  // aka "list cons"
  func push<T>(x : T, l : List<T>) : List<T> =
    ?(x, l);

  // last element, optionally; tail recursive
  func last<T>(l : List<T>) : ?T = {
    switch l {
    case null        { null };
    case (?(x,null)) { ?x };
    case (?(_,t))    { last<T>(t) };
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
    case (_, null)     { null };
    case (0, (?(h,t))) { ?h };
    case (_, (?(_,t))) { nth<T>(t, n - 1) };
    }
  };

  // reverse; tail recursive
  func rev<T>(l : List<T>) : List<T> = {
    func rec(l : List<T>, r : List<T>) : List<T> {
      switch l {
	    case null     { r };
	    case (?(h,t)) { rec(t,?(h,r)) };
      }
    };
    rec(l, null)
  };

  // Called "app" in SML Basis, and "iter" in OCaml; tail recursive
  func iter<T>(l : List<T>, f:T -> ()) : () = {
    func rec(l : List<T>) : () {
      switch l {
	    case null     { () };
	    case (?(h,t)) { f(h) ; rec(t) };
      }
    };
    rec(l)
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
  func mapFilter<T,S>(l : List<T>, f:T -> ?S) : List<S> = {
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

  // concat (aka "list join"); tail recursive, but requires "two passes"
  func concat<T>(l : List<List<T>>) : List<T> = {
    // 1/2: fold from left to right, reverse-appending the sublists...
    let r =
      { let f = func(a:List<T>, b:List<T>) : List<T> { revAppend<T>(a,b) };
	      foldLeft<List<T>, List<T>>(l, null, f)
      };
    // 2/2: ...re-reverse the elements, to their original order:
    rev<T>(r)
  };

  // (See SML Basis library); tail recursive
  func revAppend<T>(l1 : List<T>, l2 : List<T>) : List<T> = {
    switch l1 {
    case null     { l2 };
    case (?(h,t)) { revAppend<T>(t, ?(h,l2)) };
    }
  };

  // take; non-tail recursive
  // (Note: need mutable Cons tails for tail-recursive version)
  func take<T>(l : List<T>, n:Nat) : List<T> = {
    switch (l, n) {
    case (_, 0) { null };
    case (null,_) { null };
    case (?(h, t), m) {?(h, take<T>(t, m - 1))};
    }
  };

  // drop; tail recursive
  func drop<T>(l : List<T>, n:Nat) : List<T> = {
    switch (l, n) {
    case (l_,     0) { l_ };
    case (null,   _) { null };
    case ((?(h,t)), m) { drop<T>(t, m - 1) };
    }
  };

  // fold list left-to-right using f; tail recursive
  func foldLeft<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S = {
    func rec(l:List<T>, a:S) : S = {
      switch l {
      case null     { a };
      case (?(h,t)) { rec(t, f(h,a)) };
      }
    };
    rec(l,a)
  };

  // fold list right-to-left using f; non-tail recursive
  func foldRight<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S = {
    func rec(l:List<T>) : S = {
      switch l {
      case null     { a };
      case (?(h,t)) { f(h, rec(t)) };
      }
    };
    rec(l)
  };

  // test if there exists list element for which given predicate is true
  func find<T>(l: List<T>, f:T -> Bool) : ?T = {
    func rec(l:List<T>) : ?T {
      switch l {
	    case null     { null };
	    case (?(h,t)) { if (f(h)) { ?h } else { rec(t) } };
      }
    };
    rec(l)
  };

  // test if there exists list element for which given predicate is true
  func exists<T>(l: List<T>, f:T -> Bool) : Bool = {
    func rec(l:List<T>) : Bool {
      switch l {
	    case null     { false };
	    // XXX/minor --- Missing parens on condition leads to unhelpful error:
	    //case (?(h,t)) { if f(h) { true } else { rec(t) } };
	    case (?(h,t)) { if (f(h)) { true } else { rec(t) } };
      }
    };
    rec(l)
  };

  // test if given predicate is true for all list elements
  func all<T>(l: List<T>, f:T -> Bool) : Bool = {
    func rec(l:List<T>) : Bool {
      switch l {
	    case null     { true };
	    case (?(h,t)) { if (f(h)) { false } else { rec(t) } };
      }
    };
    rec(l)
  };

  // Given two ordered lists, merge them into a single ordered list
  func merge<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : List<T> {
    func rec(l1: List<T>, l2: List<T>) : List<T> {
      switch (l1, l2) {
	    case (null, _) { l2 };
	    case (_, null) { l1 };
	    case (?(h1,t1), ?(h2,t2)) {
	           if (lte(h1,h2)) {
		           ?(h1, rec(t1, ?(h2,t2)))
	           } else {
		           ?(h2, rec(?(h1,t1), t2))
	           }
	         };
      }
    };
    rec(l1, l2)
  };

  // Compare two lists lexicographic` ordering. tail recursive.
  // XXX: Eventually, follow `collate` design from SML Basis, with real sum types, use 3-valued `order` type here.
  //
  func lessThanEq<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : Bool {
    func rec(l1: List<T>, l2: List<T>) : Bool {
      switch (l1, l2) {
	    case (null, _) { true };
	    case (_, null) { false };
	    case (?(h1,t1), ?(h2,t2)) { lte(h1,h2) and rec(t1, t2) };
      }
    };
    rec(l1, l2)
  };

  // Compare two lists for equality. tail recursive.
  // `isEq(l1, l2)` =equiv= `lessThanEq(l1,l2) && lessThanEq(l2,l1)`, but the former is more efficient.
  func isEq<T>(l1: List<T>, l2: List<T>, eq:(T,T) -> Bool) : Bool {
    func rec(l1: List<T>, l2: List<T>) : Bool {
      switch (l1, l2) {
	    case (null, null) { true };
	    case (null, _)    { false };
	    case (_,    null) { false };
	    case (?(h1,t1), ?(h2,t2)) { eq(h1,h2) and rec(t1, t2) };
      }
    };
    rec(l1, l2)
  };

  // using a predicate, create two lists from one: the "true" list, and the "false" list.
  // (See SML basis library); non-tail recursive
  func partition<T>(l: List<T>, f:T -> Bool) : (List<T>, List<T>) {
    func rec(l: List<T>) : (List<T>, List<T>) {
      switch l {
	    case null { (null, null) };
	    case (?(h,t)) {
	           let (pl,pr) = rec(t);
	           if (f(h)) {
		           (?(h, pl), pr)
	           } else {
		           (pl, ?(h, pr))
	           }
	         };
      }
    };
    rec(l)
  };

  // generate a list based on a length, and a function from list index to list element;
  // (See SML basis library); non-tail recursive
  func tabulate<T>(n:Nat, f:Nat -> T) : List<T> {
    func rec(i:Nat) : List<T> {
      if (i == n) { null } else { ?(f(i), rec(i+1)) }
    };
    rec(0)
  };

};
