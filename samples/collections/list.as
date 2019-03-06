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

// empty list
func List__nil<T>() : List<T> =
  null;

// test for empty list
func List__isNil<T>(l : List<T>) : Bool {
  switch l {
  case null { true  };
  case _    { false };
  }
};

// aka "list cons"
func List__push<T>(x : T, l : List<T>) : List<T> =
  ?(x, l);

// last element, optionally; tail recursive
func List__last<T>(l : List<T>) : ?T = {
  switch l {
    case null        { null };
    case (?(x,null)) { ?x };
    case (?(_,t))    { List__last<T>(t) };
  }
};

// treat the list as a stack; combines 'hd' and (non-failing) 'tl' into one operation
func List__pop<T>(l : List<T>) : (?T, List<T>) = {
  switch l {
  case null      { (null, null) };
  case (?(h, t)) { (?h, t) };
  }
};

// length; tail recursive
func List__len<T>(l : List<T>) : Nat = {
  func rec(l : List<T>, n : Nat) : Nat {
    switch l {
      case null     { n };
      case (?(_,t)) { rec(t,n+1) };
    }
  };
  rec(l,0)
};

// array-like list access, but in linear time; tail recursive
func List__nth<T>(l : List<T>, n : Nat) : ?T = {
  switch (n, l) {
  case (_, null)     { null };
  case (0, (?(h,t))) { ?h };
  case (_, (?(_,t))) { List__nth<T>(t, n - 1) };
  }
};

// reverse; tail recursive
func List__rev<T>(l : List<T>) : List<T> = {
  func rec(l : List<T>, r : List<T>) : List<T> {
    switch l {
      case null     { r };
      case (?(h,t)) { rec(t,?(h,r)) };
    }
  };
  rec(l, null)
};

// Called "app" in SML Basis, and "iter" in OCaml; tail recursive
func List__iter<T>(l : List<T>, f:T -> ()) : () = {
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
func List__map<T,S>(l : List<T>, f:T -> S) : List<S> = {
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
func List__filter<T>(l : List<T>, f:T -> Bool) : List<T> = {
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
func List__mapFilter<T,S>(l : List<T>, f:T -> ?S) : List<S> = {
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
func List__append<T>(l : List<T>, m : List<T>) : List<T> = {
  func rec(l : List<T>) : List<T> {
    switch l {
    case null     { m };
    case (?(h,t)) {?(h,rec(l))};
    }
  };
  rec(l)
};

// concat (aka "list join"); tail recursive, but requires "two passes"
func List__concat<T>(l : List<List<T>>) : List<T> = {
  // 1/2: fold from left to right, reverse-appending the sublists...
  let r =
    { let f = func(a:List<T>, b:List<T>) : List<T> { List__revAppend<T>(a,b) };
      List__foldLeft<List<T>, List<T>>(l, null, f)
    };
  // 2/2: ...re-reverse the elements, to their original order:
  List__rev<T>(r)
};

// (See SML Basis library); tail recursive
func List__revAppend<T>(l1 : List<T>, l2 : List<T>) : List<T> = {
  switch l1 {
    case null     { l2 };
    case (?(h,t)) { List__revAppend<T>(t, ?(h,l2)) };
  }
};

// take; non-tail recursive
// (Note: need mutable Cons tails for tail-recursive version)
func List__take<T>(l : List<T>, n:Nat) : List<T> = {
  switch (l, n) {
  case (_, 0) { null };
  case (null,_) { null };
  case (?(h, t), m) {?(h, List__take<T>(t, m - 1))};
  }
};

// drop; tail recursive
func List__drop<T>(l : List<T>, n:Nat) : List<T> = {
  switch (l, n) {
  case (l_,     0) { l_ };
  case (null,   _) { null };
  case ((?(h,t)), m) { List__drop<T>(t, m - 1) };
  }
};

// fold list left-to-right using f; tail recursive
func List__foldLeft<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S = {
  func rec(l:List<T>, a:S) : S = {
    switch l {
    case null     { a };
    case (?(h,t)) { rec(t, f(h,a)) };
    }
  };
  rec(l,a)
};

// fold list right-to-left using f; non-tail recursive
func List__foldRight<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S = {
  func rec(l:List<T>) : S = {
    switch l {
    case null     { a };
    case (?(h,t)) { f(h, rec(t)) };
    }
  };
  rec(l)
};

// test if there exists list element for which given predicate is true
func List__find<T>(l: List<T>, f:T -> Bool) : ?T = {
  func rec(l:List<T>) : ?T {
    switch l {
      case null     { null };
      case (?(h,t)) { if (f(h)) { ?h } else { rec(t) } };
    }
  };
  rec(l)
};

// test if there exists list element for which given predicate is true
func List__exists<T>(l: List<T>, f:T -> Bool) : Bool = {
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
func List__all<T>(l: List<T>, f:T -> Bool) : Bool = {
  func rec(l:List<T>) : Bool {
    switch l {
      case null     { true };
      case (?(h,t)) { if (f(h)) { false } else { rec(t) } };
    }
  };
  rec(l)
};

// Given two ordered lists, merge them into a single ordered list
func List__merge<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : List<T> {
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
func List__lessThanEq<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : Bool {
  func rec(l1: List<T>, l2: List<T>) : Bool {
    switch (l1, l2) {
      case (null, _) { true };
      case (_, null) { false };
      case (?(h1,t1), ?(h2,t2)) {
             if (lte(h1,h2)) {
               rec(t1, t2)
             } else {
               false
             }
           };
    }
  };
  rec(l1, l2)
};

// Compare two lists for equality. tail recursive.
// `isEq(l1, l2)` =equiv= `lessThanEq(l1,l2) && lessThanEq(l2,l1)`, but the former is more efficient.
func List__isEq<T>(l1: List<T>, l2: List<T>, eq:(T,T) -> Bool) : Bool {
  func rec(l1: List<T>, l2: List<T>) : Bool {
    switch (l1, l2) {
      case (null, null) { true };
      case (null, _)    { false };
      case (_,    null) { false };
      case (?(h1,t1), ?(h2,t2)) {
             if (eq(h1,h2)) {
               rec(t1, t2)
             } else {
               false
             }
           };
    }
  };
  rec(l1, l2)
};

// using a predicate, create two lists from one: the "true" list, and the "false" list.
// (See SML basis library); non-tail recursive
func List__partition<T>(l: List<T>, f:T -> Bool) : (List<T>, List<T>) {
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
func List__tabulate<T>(n:Nat, f:Nat -> T) : List<T> {
  func rec(i:Nat) : List<T> {
    if (i == n) { null } else { ?(f(i), rec(i+1)) }
  };
  rec(0)
};


// Create a record,
// as a standin until we have "real" modules to create namespaces:
let List = new {
  moduleName = "List"
  ; nil        = List__nil
  ; isNil      = List__isNil
  ; push       = List__push
  ; last       = List__last
  ; pop        = List__pop
  ; len        = List__len
  ; nth        = List__nth
  ; rev        = List__rev
  ; iter       = List__iter
  ; filter     = List__filter
  ; mapFilter  = List__mapFilter
  ; append     = List__append
  ; concat     = List__concat
  ; revAppend  = List__revAppend
  ; take       = List__take
  ; drop       = List__drop
  ; foldLeft   = List__foldLeft
  ; foldRight  = List__foldRight
  ; find       = List__find
  ; exists     = List__exists
  ; all        = List__all
  ; merge      = List__merge
  ; lessThanEq = List__lessThanEq
  ; partition  = List__partition
  ; tabulate   = List__tabulate
};

