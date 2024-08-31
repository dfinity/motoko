/// Purely-functional, singly-linked lists.

/// A list of type `List<T>` is either `null` or an optional pair of a value of type `T` and a tail, itself of type `List<T>`.
///
/// To use this library, import it using:
///
/// ```motoko name=initialize
/// import List "mo:base/List";
/// ```

import Array "Array";
import Iter "IterType";
import Option "Option";
import Order "Order";
import Result "Result";

module {

  // A singly-linked list consists of zero or more _cons cells_, wherein
  // each cell contains a single list element (the cell's _head_), and a pointer to the
  // remainder of the list (the cell's _tail_).
  public type List<T> = ?(T, List<T>);

  /// Create an empty list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.nil<Nat>() // => null
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func nil<T>() : List<T> = null;

  /// Check whether a list is empty and return true if the list is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.isNil<Nat>(null) // => true
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func isNil<T>(l : List<T>) : Bool {
    switch l {
      case null { true };
      case _ { false }
    }
  };

  /// Add `x` to the head of `list`, and return the new list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.push<Nat>(0, null) // => ?(0, null);
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func push<T>(x : T, l : List<T>) : List<T> = ?(x, l);

  /// Return the last element of the list, if present.
  /// Example:
  /// ```motoko include=initialize
  /// List.last<Nat>(?(0, ?(1, null))) // => ?1
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func last<T>(l : List<T>) : ?T {
    switch l {
      case null { null };
      case (?(x, null)) { ?x };
      case (?(_, t)) { last<T>(t) }
    }
  };

  /// Remove the head of the list, returning the optioned head and the tail of the list in a pair.
  /// Returns `(null, null)` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.pop<Nat>(?(0, ?(1, null))) // => (?0, ?(1, null))
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func pop<T>(l : List<T>) : (?T, List<T>) {
    switch l {
      case null { (null, null) };
      case (?(h, t)) { (?h, t) }
    }
  };

  /// Return the length of the list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.size<Nat>(?(0, ?(1, null))) // => 2
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func size<T>(l : List<T>) : Nat {
    func rec(l : List<T>, n : Nat) : Nat {
      switch l {
        case null { n };
        case (?(_, t)) { rec(t, n + 1) }
      }
    };
    rec(l, 0)
  };
  /// Access any item in a list, zero-based.
  ///
  /// NOTE: Indexing into a list is a linear operation, and usually an
  /// indication that a list might not be the best data structure
  /// to use.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.get<Nat>(?(0, ?(1, null)), 1) // => ?1
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func get<T>(l : List<T>, n : Nat) : ?T {
    switch (n, l) {
      case (_, null) { null };
      case (0, (?(h, t))) { ?h };
      case (_, (?(_, t))) { get<T>(t, n - 1) }
    }
  };

  /// Reverses the list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.reverse<Nat>(?(0, ?(1, ?(2, null)))) // => ?(2, ?(1, ?(0, null)))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func reverse<T>(l : List<T>) : List<T> {
    func rec(l : List<T>, r : List<T>) : List<T> {
      switch l {
        case null { r };
        case (?(h, t)) { rec(t, ?(h, r)) }
      }
    };
    rec(l, null)
  };

  /// Call the given function for its side effect, with each list element in turn.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// var sum = 0;
  /// List.iterate<Nat>(?(0, ?(1, ?(2, null))), func n { sum += n });
  /// sum // => 3
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func iterate<T>(l : List<T>, f : T -> ()) {
    switch l {
      case null { () };
      case (?(h, t)) { f(h); iterate<T>(t, f) }
    }
  };

  /// Call the given function `f` on each list element and collect the results
  /// in a new list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat = "mo:base/Nat"
  /// List.map<Nat, Text>(?(0, ?(1, ?(2, null))), Nat.toText) // => ?("0", ?("1", ?("2", null))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func map<T, U>(l : List<T>, f : T -> U) : List<U> {
    switch l {
      case null { null };
      case (?(h, t)) { ?(f(h), map<T, U>(t, f)) }
    }
  };

  /// Create a new list with only those elements of the original list for which
  /// the given function (often called the _predicate_) returns true.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.filter<Nat>(?(0, ?(1, ?(2, null))), func n { n != 1 }) // => ?(0, ?(2, null))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func filter<T>(l : List<T>, f : T -> Bool) : List<T> {
    switch l {
      case null { null };
      case (?(h, t)) {
        if (f(h)) {
          ?(h, filter<T>(t, f))
        } else {
          filter<T>(t, f)
        }
      }
    }
  };

  /// Create two new lists from the results of a given function (`f`).
  /// The first list only includes the elements for which the given
  /// function `f` returns true and the second list only includes
  /// the elements for which the function returns false.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.partition<Nat>(?(0, ?(1, ?(2, null))), func n { n != 1 }) // => (?(0, ?(2, null)), ?(1, null))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func partition<T>(l : List<T>, f : T -> Bool) : (List<T>, List<T>) {
    switch l {
      case null { (null, null) };
      case (?(h, t)) {
        if (f(h)) {
          // call f in-order
          let (l, r) = partition<T>(t, f);
          (?(h, l), r)
        } else {
          let (l, r) = partition<T>(t, f);
          (l, ?(h, r))
        }
      }
    }
  };

  /// Call the given function on each list element, and collect the non-null results
  /// in a new list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.mapFilter<Nat, Nat>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   func n {
  ///     if (n > 1) {
  ///       ?(n * 2);
  ///     } else {
  ///       null
  ///     }
  ///   }
  /// ) // => ?(4, ?(6, null))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func mapFilter<T, U>(l : List<T>, f : T -> ?U) : List<U> {
    switch l {
      case null { null };
      case (?(h, t)) {
        switch (f(h)) {
          case null { mapFilter<T, U>(t, f) };
          case (?h_) { ?(h_, mapFilter<T, U>(t, f)) }
        }
      }
    }
  };

  /// Maps a Result-returning function `f` over a List and returns either
  /// the first error or a list of successful values.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.mapResult<Nat, Nat, Text>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   func n {
  ///     if (n > 0) {
  ///       #ok(n * 2);
  ///     } else {
  ///       #err("Some element is zero")
  ///     }
  ///   }
  /// ); // => #ok ?(2, ?(4, ?(6, null))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapResult<T, R, E>(xs : List<T>, f : T -> Result.Result<R, E>) : Result.Result<List<R>, E> {
    func go(xs : List<T>, acc : List<R>) : Result.Result<List<R>, E> {
      switch xs {
        case null { #ok(acc) };
        case (?(head, tail)) {
          switch (f(head)) {
            case (#err(err)) { #err(err) };
            case (#ok(ok)) { go(tail, ?(ok, acc)) }
          }
        }
      }
    };
    Result.mapOk(go(xs, null), func(xs : List<R>) : List<R> = reverse(xs))
  };

  /// Append the elements from the reverse of one list, 'l', to another list, 'm'.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.revAppend<Nat>(
  ///   ?(2, ?(1, ?(0, null))),
  ///   ?(3, ?(4, ?(5, null)))
  /// ); // => ?(0, ?(1, ?(2, ?(3, ?(4, ?(5, null))))))
  /// ```
  ///
  /// Runtime: O(size(l))
  ///
  /// Space: O(size(l))
  func revAppend<T>(l : List<T>, m : List<T>) : List<T> {
    switch l {
      case null { m };
      case (?(h, t)) { revAppend(t, ?(h, m)) }
    }
  };

  /// Append the elements from one list to another list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.append<Nat>(
  ///   ?(0, ?(1, ?(2, null))),
  ///   ?(3, ?(4, ?(5, null)))
  /// ) // => ?(0, ?(1, ?(2, ?(3, ?(4, ?(5, null))))))
  /// ```
  ///
  /// Runtime: O(size(l))
  ///
  /// Space: O(size(l))
  public func append<T>(l : List<T>, m : List<T>) : List<T> {
    revAppend(reverse(l), m)
  };

  /// Flatten, or concatenate, a list of lists as a list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.flatten<Nat>(
  ///   ?(?(0, ?(1, ?(2, null))),
  ///     ?(?(3, ?(4, ?(5, null))),
  ///       null))
  /// ); // => ?(0, ?(1, ?(2, ?(3, ?(4, ?(5, null))))))
  /// ```
  ///
  /// Runtime: O(size*size)
  ///
  /// Space: O(size*size)
  public func flatten<T>(l : List<List<T>>) : List<T> {
  //FIXME: this is quadratic, not linear https://github.com/dfinity/motoko-base/issues/459
    foldLeft<List<T>, List<T>>(l, null, func(a, b) { append<T>(a, b) })
  };

  /// Returns the first `n` elements of the given list.
  /// If the given list has fewer than `n` elements, this function returns
  /// a copy of the full input list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.take<Nat>(
  ///   ?(0, ?(1, ?(2, null))),
  ///   2
  /// ); // => ?(0, ?(1, null))
  /// ```
  ///
  /// Runtime: O(n)
  ///
  /// Space: O(n)
  public func take<T>(l : List<T>, n : Nat) : List<T> {
    switch (l, n) {
      case (_, 0) { null };
      case (null, _) { null };
      case (?(h, t), m) { ?(h, take<T>(t, m - 1)) }
    }
  };

  /// Drop the first `n` elements from the given list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.drop<Nat>(
  ///   ?(0, ?(1, ?(2, null))),
  ///   2
  /// ); // => ?(2, null)
  /// ```
  ///
  /// Runtime: O(n)
  ///
  /// Space: O(1)
  public func drop<T>(l : List<T>, n : Nat) : List<T> {
    switch (l, n) {
      case (l_, 0) { l_ };
      case (null, _) { null };
      case ((?(h, t)), m) { drop<T>(t, m - 1) }
    }
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// List.foldLeft<Nat, Text>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   "",
  ///   func (acc, x) { acc # Nat.toText(x)}
  /// ) // => "123"
  /// ```
  ///
  /// Runtime: O(size(list))
  ///
  /// Space: O(1) heap, O(1) stack
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldLeft<T, S>(list : List<T>, base : S, combine : (S, T) -> S) : S {
    switch list {
      case null { base };
      case (?(h, t)) { foldLeft(t, combine(base, h), combine) }
    }
  };

  /// Collapses the elements in `buffer` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// List.foldRight<Nat, Text>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   "",
  ///   func (x, acc) { Nat.toText(x) # acc}
  /// ) // => "123"
  /// ```
  ///
  /// Runtime: O(size(list))
  ///
  /// Space: O(1) heap, O(size(list)) stack
  ///
  /// *Runtime and space assumes that `combine` runs in O(1) time and space.
  public func foldRight<T, S>(list : List<T>, base : S, combine : (T, S) -> S) : S {
    switch list {
      case null { base };
      case (?(h, t)) { combine(h, foldRight<T, S>(t, base, combine)) }
    }
  };

  /// Return the first element for which the given predicate `f` is true,
  /// if such an element exists.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// List.find<Nat>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   func n { n > 1 }
  /// ); // => ?2
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func find<T>(l : List<T>, f : T -> Bool) : ?T {
    switch l {
      case null { null };
      case (?(h, t)) { if (f(h)) { ?h } else { find<T>(t, f) } }
    }
  };

  /// Return true if there exists a list element for which
  /// the given predicate `f` is true.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// List.some<Nat>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   func n { n > 1 }
  /// ) // => true
  /// ```
  ///
  /// Runtime: O(size(list))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func some<T>(l : List<T>, f : T -> Bool) : Bool {
    switch l {
      case null { false };
      case (?(h, t)) { f(h) or some<T>(t, f) }
    }
  };

  /// Return true if the given predicate `f` is true for all list
  /// elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// List.all<Nat>(
  ///   ?(1, ?(2, ?(3, null))),
  ///   func n { n > 1 }
  /// ); // => false
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func all<T>(l : List<T>, f : T -> Bool) : Bool {
    switch l {
      case null { true };
      case (?(h, t)) { f(h) and all<T>(t, f) }
    }
  };

  /// Merge two ordered lists into a single ordered list.
  /// This function requires both list to be ordered as specified
  /// by the given relation `lessThanOrEqual`.
  ///
  /// Example:
  /// ```motoko include=initialize
  ///
  /// List.merge<Nat>(
  ///   ?(1, ?(2, ?(4, null))),
  ///   ?(2, ?(4, ?(6, null))),
  ///   func (n1, n2) { n1 <= n2 }
  /// ); // => ?(1, ?(2, ?(2, ?(4, ?(4, ?(6, null))))))),
  /// ```
  ///
  /// Runtime: O(size(l1) + size(l2))
  ///
  /// Space: O(size(l1) + size(l2))
  ///
  /// *Runtime and space assumes that `lessThanOrEqual` runs in O(1) time and space.
  // TODO: replace by merge taking a compare : (T, T) -> Order.Order function?
  public func merge<T>(l1 : List<T>, l2 : List<T>, lessThanOrEqual : (T, T) -> Bool) : List<T> {
    switch (l1, l2) {
      case (null, _) { l2 };
      case (_, null) { l1 };
      case (?(h1, t1), ?(h2, t2)) {
        if (lessThanOrEqual(h1, h2)) {
          ?(h1, merge<T>(t1, l2, lessThanOrEqual))
        } else {
          ?(h2, merge<T>(l1, t2, lessThanOrEqual))
        }
      }
    }
  };

  private func compareAux<T>(l1 : List<T>, l2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order {
    switch (l1, l2) {
      case (null, null) { #equal };
      case (null, _) { #less };
      case (_, null) { #greater };
      case (?(h1, t1), ?(h2, t2)) {
        switch (compare(h1, h2)) {
          case (#equal) { compareAux<T>(t1, t2, compare) };
          case other { other }
        }
      }
    }
  };

  /// Compare two lists using lexicographic ordering specified by argument function `compare`.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// List.compare<Nat>(
  ///   ?(1, ?(2, null)),
  ///   ?(3, ?(4, null)),
  ///   Nat.compare
  /// ) // => #less
  /// ```
  ///
  /// Runtime: O(size(l1))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that argument `compare` runs in O(1) time and space.
  public func compare<T>(l1 : List<T>, l2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order {
     compareAux<T>(l1, l2, compare);
  };

  private func equalAux<T>(l1 : List<T>, l2 : List<T>, equal : (T, T) -> Bool) : Bool {
    switch (l1, l2) {
      case (?(h1, t1), ?(h2, t2)) {
        equal(h1, h2) and equalAux<T>(t1, t2, equal)
      };
      case (null, null) { true };
      case _ { false };
    }
  };
  /// Compare two lists for equality using the argument function `equal` to determine equality of their elements.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat "mo:base/Nat";
  ///
  /// List.equal<Nat>(
  ///   ?(1, ?(2, null)),
  ///   ?(3, ?(4, null)),
  ///   Nat.equal
  /// ); // => false
  /// ```
  ///
  /// Runtime: O(size(l1))
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that argument `equal` runs in O(1) time and space.
  public func equal<T>(l1 : List<T>, l2 : List<T>, equal : (T, T) -> Bool) : Bool {
    equalAux<T>(l1, l2, equal);
  };

  /// Generate a list based on a length and a function that maps from
  /// a list index to a list element.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.tabulate<Nat>(
  ///   3,
  ///   func n { n * 2 }
  /// ) // => ?(0, ?(2, (?4, null)))
  /// ```
  ///
  /// Runtime: O(n)
  ///
  /// Space: O(n)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func tabulate<T>(n : Nat, f : Nat -> T) : List<T> {
    var i = 0;
    var l : List<T> = null;
    while (i < n) {
      l := ?(f(i), l);
      i += 1
    };
    reverse(l)
  };

  /// Create a list with exactly one element.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.make<Nat>(
  ///   0
  /// ) // => ?(0, null)
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func make<T>(x : T) : List<T> = ?(x, null);

  /// Create a list of the given length with the same value in each position.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.replicate<Nat>(
  ///   3,
  ///   0
  /// ) // => ?(0, ?(0, ?(0, null)))
  /// ```
  ///
  /// Runtime: O(n)
  ///
  /// Space: O(n)
  public func replicate<T>(n : Nat, x : T) : List<T> {
    var i = 0;
    var l : List<T> = null;
    while (i < n) {
      l := ?(x, l);
      i += 1
    };
    l
  };

  /// Create a list of pairs from a pair of lists.
  ///
  /// If the given lists have different lengths, then the created list will have a
  /// length equal to the length of the smaller list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.zip<Nat, Text>(
  ///   ?(0, ?(1, ?(2, null))),
  ///   ?("0", ?("1", null)),
  /// ) // => ?((0, "0"), ?((1, "1"), null))
  /// ```
  ///
  /// Runtime: O(min(size(xs), size(ys)))
  ///
  /// Space: O(min(size(xs), size(ys)))
  public func zip<T, U>(xs : List<T>, ys : List<U>) : List<(T, U)> = zipWith<T, U, (T, U)>(xs, ys, func(x, y) { (x, y) });

  /// Create a list in which elements are created by applying function `f` to each pair `(x, y)` of elements
  /// occuring at the same position in list `xs` and list `ys`.
  ///
  /// If the given lists have different lengths, then the created list will have a
  /// length equal to the length of the smaller list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// import Nat = "mo:base/Nat";
  /// import Char = "mo:base/Char";
  ///
  /// List.zipWith<Nat, Char, Text>(
  ///   ?(0, ?(1, ?(2, null))),
  ///   ?('a', ?('b', null)),
  ///   func (n, c) { Nat.toText(n) # Char.toText(c) }
  /// ) // => ?("0a", ?("1b", null))
  /// ```
  ///
  /// Runtime: O(min(size(xs), size(ys)))
  ///
  /// Space: O(min(size(xs), size(ys)))
  ///
  /// *Runtime and space assumes that `zip` runs in O(1) time and space.
  public func zipWith<T, U, V>(
    xs : List<T>,
    ys : List<U>,
    f : (T, U) -> V
  ) : List<V> {
    switch (pop<T>(xs)) {
      case (null, _) { null };
      case (?x, xt) {
        switch (pop<U>(ys)) {
          case (null, _) { null };
          case (?y, yt) {
            push<V>(f(x, y), zipWith<T, U, V>(xt, yt, f))
          }
        }
      }
    }
  };

  /// Split the given list at the given zero-based index.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.split<Nat>(
  ///   2,
  ///   ?(0, ?(1, ?(2, null)))
  /// ) // => (?(0, ?(1, null)), ?(2, null))
  /// ```
  ///
  /// Runtime: O(n)
  ///
  /// Space: O(n)
  ///
  /// *Runtime and space assumes that `zip` runs in O(1) time and space.
  public func split<T>(n : Nat, xs : List<T>) : (List<T>, List<T>) {
    if (n == 0) { (null, xs) } else {
      func rec(n : Nat, xs : List<T>) : (List<T>, List<T>) {
        switch (pop<T>(xs)) {
          case (null, _) { (null, null) };
          case (?h, t) {
            if (n == 1) { (make<T>(h), t) } else {
              let (l, r) = rec(n - 1, t);
              (push<T>(h, l), r)
            }
          }
        }
      };
      rec(n, xs)
    }
  };

  /// Split the given list into chunks of length `n`.
  /// The last chunk will be shorter if the length of the given list
  /// does not divide by `n` evenly.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.chunks<Nat>(
  ///   2,
  ///   ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
  /// )
  /// /* => ?(?(0, ?(1, null)),
  ///         ?(?(2, ?(3, null)),
  ///           ?(?(4, null),
  ///             null)))
  /// */
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `zip` runs in O(1) time and space.
  public func chunks<T>(n : Nat, xs : List<T>) : List<List<T>> {
    let (l, r) = split<T>(n, xs);
    if (isNil<T>(l)) {
      null
    } else {
      push<List<T>>(l, chunks<T>(n, r))
    }
  };

  /// Convert an array into a list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.fromArray<Nat>([ 0, 1, 2, 3, 4])
  /// // =>  ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func fromArray<T>(xs : [T]) : List<T> {
    Array.foldRight<T, List<T>>(
      xs,
      null,
      func(x : T, ys : List<T>) : List<T> {
        push<T>(x, ys)
      }
    )
  };

  /// Convert a mutable array into a list.
  ///
  /// Example:
  /// ```motoko include=initialize
  /// List.fromVarArray<Nat>([var 0, 1, 2, 3, 4])
  /// // =>  ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func fromVarArray<T>(xs : [var T]) : List<T> = fromArray<T>(Array.freeze<T>(xs));

  /// Create an array from a list.
  /// Example:
  /// ```motoko include=initialize
  /// List.toArray<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))
  /// // => [0, 1, 2, 3, 4]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func toArray<T>(xs : List<T>) : [T] {
    let length = size<T>(xs);
    var list = xs;
    Array.tabulate<T>(
      length,
      func(i) {
        let popped = pop<T>(list);
        list := popped.1;
        switch (popped.0) {
          case null { loop { assert false } };
          case (?x) x
        }
      }
    )
  };

  /// Create a mutable array from a list.
  /// Example:
  /// ```motoko include=initialize
  /// List.toVarArray<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))
  /// // => [var 0, 1, 2, 3, 4]
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  public func toVarArray<T>(xs : List<T>) : [var T] = Array.thaw<T>(toArray<T>(xs));

  /// Create an iterator from a list.
  /// Example:
  /// ```motoko include=initialize
  /// var sum = 0;
  /// for (n in List.toIter<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))) {
  ///   sum += n;
  /// };
  /// sum
  /// // => 10
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func toIter<T>(xs : List<T>) : Iter.Iter<T> {
    var state = xs;
    object {
      public func next() : ?T = switch state {
        case (?(hd, tl)) { state := tl; ?hd };
        case _ null
      }
    }
  }

}
