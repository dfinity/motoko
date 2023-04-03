module {
/**

# List

Purely-functional, singly-linked lists.

*/

/**
 Representation
 =================

 A singly-linked list consists of zero or more _cons cells_, wherein
each cell contains a single list element (the cell's _head_), and a pointer to the
remainder of the list (the cell's _tail_).

*/

public type List<T> = ?(T, List<T>);

/**
 Interface
 ==============
*/

  /**
   `nil`
   ------
   empty list
   */
  public func nil<T>() : List<T> =
    null;


  /**
   `isNil`
   --------
   test for empty list
   */
  public func isNil<T>(l : List<T>) : Bool {
    switch l {
    case null { true  };
    case _    { false };
    }
  };

  /// Documentation for `push`
  public func push<T>(x : T, l : List<T>) : List<T> =
    ?(x, l);

  /**
   `last`
   ----------
   last element, optionally; tail recursive
   */
  public func last<T>(l : List<T>) : ?T {
    switch l {
    case null        { null };
    case (?(x,null)) { ?x };
    case (?(_,t))    { last<T>(t) };
    }
  };

  /**
   `pop`
   --------
   treat the list as a stack; combines the usual operations `head` and (non-failing) `tail` into one operation
   */
  public func pop<T>(l : List<T>) : (?T, List<T>) {
    switch l {
    case null      { (null, null) };
    case (?(h, t)) { (?h, t) };
    }
  };

  /**
   `len`
  --------
   length; tail recursive
   */
  public func len<T>(l : List<T>) : Nat = label profile_list_len : Nat {
    func rec(l : List<T>, n : Nat) : Nat = label profile_list_len_rec : Nat {
      switch l {
      case null     { n };
      case (?(_,t)) { rec(t,n+1) };
      }
    };
    rec(l,0)
  };

  /**
   `lenIsLessThan`
  --------
   test length against a maximum value; tail recursive
   */
  public func lenIsEqLessThan<T>(l : List<T>, i : Nat) : Bool =
    label profile_list_lenIsEqLessThan_begin : Bool {
    func rec(l : List<T>, i : Nat) : Bool = label profile_list_lenIsEqLessThan_rec : Bool {
      switch l {
	    case null label profile_list_lenIsEqLessThan_end_true : Bool true;
	    case (?(_, t)) {
             if ( i == 0 ) {
               label profile_list_lenIsEqLessThan_end_false : Bool
               false
             }
             else {
               rec(t, i - 1)
             }
           };
      }
    };
    rec(l, i)
  };

  /**
   `lenClamp`
  --------
   get the length, unless greater than a maximum value, in which return null; tail recursive
   */
  public func lenClamp<T>(l : List<T>, i0 : Nat) : ?Nat =
    label profile_list_lenClamp : (?Nat) {
    func rec(l : List<T>, i : Nat) : ?Nat = label profile_list_lenClamp_rec : (?Nat) {
      switch l {
	    case null { label profile_list_lenClamp_end_some : (?Nat) ?(i0 - i) };
	    case (?(_, t)) {
             if ( i == 0 ) {
               label profile_list_lenClamp_end_null : (?Nat)
               null
             }
             else {
               rec(t, i - 1)
             }
           };
      }
    };
    rec(l, i0)
  };

  /**
   `nth`
   ---------
   array-like list access, but in linear time; tail recursive
   */
  public func nth<T>(l : List<T>, n : Nat) : ?T {
    switch (n, l) {
    case (_, null)     { null };
    case (0, (?(h,t))) { ?h };
    case (_, (?(_,t))) { nth<T>(t, n - 1) };
    }
  };

  /**
   `rev`
   --------
   reverse the list; tail recursive
   */
  public func rev<T>(l : List<T>) : List<T> {
    func rec(l : List<T>, r : List<T>) : List<T> {
      switch l {
	    case null     { r };
	    case (?(h,t)) { rec(t,?(h,r)) };
      }
    };
    rec(l, null)
  };

  /**
   `iter`
   ---------
   Called `app` in SML Basis, and `iter` in OCaml; tail recursive
   */
  public func iter<T>(l : List<T>, f:T -> ()) : () {
    func rec(l : List<T>) : () {
      switch l {
	    case null     { () };
	    case (?(h,t)) { f(h) ; rec(t) };
      }
    };
    rec(l)
  };

  /**
   `map`
   ---------
   map the list elements; non-tail recursive

   Note: need mutable Cons tails for tail-recursive map.
   */
  public func map<T,S>(l : List<T>, f:T -> S) : List<S> {
    func rec(l : List<T>) : List<S> {
      switch l {
	    case null     { null };
	    case (?(h,t)) { ?(f(h),rec(t)) };
      }
    };
    rec(l)
  };

  /**
   `filter`
   ----------
   filter the list elements; non-tail recursive
   */
  public func filter<T>(l : List<T>, f:T -> Bool) : List<T> {
    func rec(l : List<T>) : List<T> {
      switch l {
	    case null     { null };
	    case (?(h,t)) { if (f(h)){ ?(h,rec(t)) } else { rec(t) } };
      }
    };
    rec(l)
  };

  /**
   `split`
   ----------
   split the list elements; non-tail recursive
   */
  public func split<T>(l : List<T>, f:T -> Bool) : (List<T>, List<T>) {
    func rec(l : List<T>) : (List<T>, List<T>) =
      label profile_list_split_rec : (List<T>, List<T>) {
      switch l {
	    case null     { (null, null) };
	    case (?(h,t)) { let (l,r) = rec(t) ;
                      if (f(h)){ (?(h,l), r) } else { (l, ?(h,r)) } };
      }
    };
    label profile_list_split_begin : (List<T>, List<T>)
    rec(l)
  };

  /**
   `mapFilter`
   --------------
   map and filter the list elements; non-tail recursive
   */
  public func mapFilter<T,S>(l : List<T>, f:T -> ?S) : List<S> {
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

  /**
   `append`
   ---------
   append two lists; non-tail recursive
   */
  public func append<T>(l : List<T>, m : List<T>) : List<T> {
    func rec(l : List<T>) : List<T> {
      switch l {
      case null     { m };
      case (?(h,t)) {?(h,rec(l))};
      }
    };
    rec(l)
  };

  /**
   `concat`
   -----------
   concat (aka "list join"); tail recursive, but requires "two passes"
   */
  public func concat<T>(l : List<List<T>>) : List<T> {
    // 1/2: fold from left to right, reverse-appending the sublists...
    let r = do
      { let f = func(a:List<T>, b:List<T>) : List<T> { revAppend<T>(a,b) };
	      foldLeft<List<T>, List<T>>(l, null, f)
      };
    // 2/2: ...re-reverse the elements, to their original order:
    rev<T>(r)
  };

  /**
   `revAppend`
   -------------
   See SML Basis library; tail recursive
   */
  public func revAppend<T>(l1 : List<T>, l2 : List<T>) : List<T> {
    switch l1 {
    case null     { l2 };
    case (?(h,t)) { revAppend<T>(t, ?(h,l2)) };
    }
  };

  /**
   `take`
   ---------
   "take" `n` elements from the prefix of the given list.
   If the given list has fewer than `n` elements, we return the full input list.
   */
  public func take<T>(l : List<T>, n:Nat) : List<T> {
    switch (l, n) {
    case (_, 0) { null };
    case (null,_) { null };
    case (?(h, t), m) {?(h, take<T>(t, m - 1))};
    }
  };

  /**
   `drop`
   ----------
   */
  public func drop<T>(l : List<T>, n:Nat) : List<T> {
    switch (l, n) {
    case (l_,     0) { l_ };
    case (null,   _) { null };
    case ((?(h,t)), m) { drop<T>(t, m - 1) };
    }
  };

  /**
   `foldLeft`
   ---------------
   fold list left-to-right using function `f`; tail recursive
   */
  public func foldLeft<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S {
    func rec(l:List<T>, a:S) : S {
      switch l {
      case null     { a };
      case (?(h,t)) { rec(t, f(h,a)) };
      }
    };
    rec(l,a)
  };

  /***
   `foldRight`
   ------------
   fold the list right-to-left using function `f`; non-tail recursive
   */
  public func foldRight<T,S>(l : List<T>, a:S, f:(T,S) -> S) : S {
    func rec(l:List<T>) : S {
      switch l {
      case null     { a };
      case (?(h,t)) { f(h, rec(t)) };
      }
    };
    rec(l)
  };

  /**
   `find`
   -----------
   test if there exists list element for which given predicate is true
   */
  public func find<T>(l: List<T>, f:T -> Bool) : ?T {
    func rec(l:List<T>) : ?T {
      switch l {
	    case null     { null };
	    case (?(h,t)) { if (f(h)) { ?h } else { rec(t) } };
      }
    };
    rec(l)
  };

  /**
   `exists`
   ---------
   test if there exists list element for which given predicate is true
   */
  public func exists<T>(l: List<T>, f:T -> Bool) : Bool {
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

  /**
   `all`
   -------
   test if given predicate is true for all list elements
   */
  public func all<T>(l: List<T>, f:T -> Bool) : Bool {
    func rec(l:List<T>) : Bool {
      switch l {
	    case null     { true };
	    case (?(h,t)) { if (not f(h)) { false } else { rec(t) } };
      }
    };
    rec(l)
  };

  /**
   `merge`
   ---------
   Given two ordered lists, merge them into a single ordered list
   */
  public func merge<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : List<T> {
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

  /**
   `lessThanEq`
   --------------

   Compare two lists lexicographic` ordering. tail recursive.

   To do: Eventually, follow `collate` design from SML Basis, with real sum types, use 3-valued `order` type here.
  */
  public func lessThanEq<T>(l1: List<T>, l2: List<T>, lte:(T,T) -> Bool) : Bool {
    func rec(l1: List<T>, l2: List<T>) : Bool {
      switch (l1, l2) {
	    case (null, _) { true };
	    case (_, null) { false };
	    case (?(h1,t1), ?(h2,t2)) { lte(h1,h2) and rec(t1, t2) };
      }
    };
    rec(l1, l2)
  };

  /**
   `isEq`
   ---------
   Compare two lists for equality. tail recursive.

   `isEq(l1, l2)` is equivalent to `lessThanEq(l1,l2) && lessThanEq(l2,l1)`, but the former is more efficient.
   */
  public func isEq<T>(l1: List<T>, l2: List<T>, eq:(T,T) -> Bool) : Bool {
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

  /**
   `partition`
   ---------------
   using a predicate, create two lists from one: the "true" list, and the "false" list.
   (See SML basis library); non-tail recursive.
   */
  public func partition<T>(l: List<T>, f:T -> Bool) : (List<T>, List<T>) {
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

  /**
   `tabulate`
   --------------
   generate a list based on a length, and a function from list index to list element.
   (See SML basis library); non-tail recursive.
   */
  public func tabulate<T>(n:Nat, f:Nat -> T) : List<T> {
    func rec(i:Nat) : List<T> {
      if (i == n) { null } else { ?(f(i), rec(i+1)) }
    };
    rec(0)
  };

/**

To do:
--------
- iterator objects, for use in `for ... in ...` patterns
- operations for lists of pairs and pairs of lists: zip, split, etc
- more regression tests for everything that is below

*/
}
