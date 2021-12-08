import Nat32 "mo:base/Nat32";
import Trie  "mo:base/Trie";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Stream "Stream";

/// Sequences with efficient, balanced representation
module {

  // Sequences with efficient, balanced representation
  //
  // #### Summary:
  //
  // - Pure/persistent representation
  // - O(log n) sequence operations (get element, split, append)
  // - Supports Deque operations in worst-case O(log n) time (base library currently uses a Deque with worst-case linear time)
  //
  // ### Algorithm details:
  //
  // See chunky list representation from [Section 5 of this POPL 1989 paper](https://dl.acm.org/doi/10.1145/75277.75305).
  //


  public type Sequence<X> = {
    #branch : Branch<X>;
    #leaf : X;
    #empty;
  };

  public type Branch<X> = {
    left : Sequence<X>;
    right : Sequence<X>;
    level : Level;
    size : Nat;
  };

  /// identify positions uniquely
  ///
  /// (generally not sequential numbers)
  public type Pos = Nat;

  /// see POPL paper
  public type Level = Nat32;

  public type Stream<X> = Stream.Stream<X>;

  public func empty<X>() : Sequence<X> {
    #empty
  };

  public func make<X>(data : X) : Sequence<X> {
    #leaf(data)
  };

  public func branch<X>(l : Sequence<X>, midLev : Level, r : Sequence<X>) : Sequence<X> {
    switch (l, r) {
      case (#empty, _) { r };
      case (_, #empty) { l };
      case (_, _) {
             let s = size(l) + size(r);
             #branch({ left = l ; level = midLev ; right = r ; size = s })
           };
    }
  };

  // given an infinite stream of levels, we can append pairs of sequences forever : )
  public func makeAppend<X>(levels : Stream<Level>) : <X>(Sequence<X>, Sequence<X>) -> Sequence<X> {
    func append<X>(s1 : Sequence<X>, s2 : Sequence<X>) : Sequence<X> {
      appendLevel(s1, levels.next(), s2)
    };
    append
  };

  public func defaultAppend<X>() : <X>(Sequence<X>, Sequence<X>) -> Sequence<X> {
    /// seed append function with a deterministic Bernoulli distribution
    makeAppend<X>(Stream.Bernoulli.seedFrom(0))
  };

  public func appendLevel<X>(s1 : Sequence<X>, midLev : Nat32, s2 : Sequence<X>) : Sequence<X> {
    switch (s1, s2) {
      case (#empty, s2) s2;
      case (s1, #empty) s1;
      case (#leaf(x), #leaf(y)) {
             branch(s1, midLev, s2)
           };
      case (#branch(b), #leaf(x)) {
             if (b.level < midLev) {
               branch(s1, midLev, s2)
             } else {
               branch(b.left, b.level, appendLevel(b.right, midLev, s2))
             }
           };
      case (#leaf(x), #branch(b)) {
             if (midLev > b.level) {
               branch(s1, midLev, s2)
             } else {
               branch(appendLevel(s1, midLev, b.left), b.level, b.right)
             }
           };
      case (#branch(b1), #branch(b2)) {
             if (midLev > b1.level and midLev > b2.level) {
               // midLevel is the max; no further recursion into s1 or s2
               branch(s1, midLev, s2)
             } else {
               // b1 or b2's level is the max level of the three; descend into the "middle child"
               if (b1.level > b2.level) {
                 branch(b1.left,
                        b1.level,
                        appendLevel(b1.right, midLev, s2)
                 )
               } else {
                 branch(appendLevel(s1, midLev, b2.left),
                        b2.level,
                        b2.right
                 )
               }
             };
           };
    }
  };

  public func fromArray<X>(array : [X], levels : Stream<Level>) : Sequence<X> {
    var s : Sequence<X> = #empty;
    for (a in array.vals()) {
      s := appendLevel<X>(s, levels.next(), make<X>(a));
    };
    s
  };

  public func size<X>(s : Sequence<X>) : Nat {
    switch s {
      case (#empty) 0;
      case (#leaf(x)) 1;
      case (#branch(b)) b.size;
    }
  };

  public func get<X>(s : Sequence<X>, pos : Nat) : ?X {
    switch s {
      case (#empty) null;
      case (#leaf(x)) if (pos == 0) ?x else null;
      case (#branch(b)) {
             let lSize = size(b.left);
             if (pos < lSize) {
               get(b.left, pos)
             } else {
               get(b.right, pos - lSize : Nat)
             }
           };
    }
  };

  /// split sequence into a pair where the first has the given size
  ///
  /// for insufficient sequence values, the first result is the full input, and the second result is empty
  public func split<X>(s : Sequence<X>, size1 : Nat) : (Sequence<X>, Sequence<X>) {
    if (size1 == 0) {
      (#empty, s)
    } else if (size1 > size(s)) {
      (s, #empty)
    } else {
      switch s {
        case (#empty) { (#empty, #empty) };
        case (#leaf(x)) { (#leaf(x), #empty) };
        case (#branch(b)) {
               if (size1 == size(b.left)) { // perfect sized match on left
                 (b.left, b.right)

               } else if (size1 < size(b.left)) { // left size is too big; split it
                 let (s1, s2) = split(b.left, size1);
                 let size1Diff = size1 - size(s1) : Nat;
                 // append, re-using old branch node's level; return extra level (if any)
                 (s1, appendLevel(s2, b.level, b.right))

               } else { // left side too small; split right and append
                 let size1Diff = size1 - size(b.left) : Nat;
                 let (s1, s2) = split(b.right, size1Diff);
                 (appendLevel(b.left, b.level, s1), s2)
               }
             }
      }
    }
  };

  public func slice<X>(s : Sequence<X>, start : Nat, size : Nat) : (Sequence<X>, Sequence<X>, Sequence<X>) {
    if (size == 0) {
      let (s1, s2) = split(s, start);
      (s1, empty(), s2)
    } else {
      let (s1, s23) = split(s, start);
      let (s2, s3) = split(s23, size);
      (s1, s2, s3)
    }
  };

  public func pushBack<X>(seq : Sequence<X>, level : Level, data : X) : Sequence<X> {
    appendLevel(seq, level, make(data))
  };

  public func popFront<X>(seq : Sequence<X>) : ?(X, Sequence<X>) {
    if (size(seq) > 0) {
      let (emp, front, rest) = slice(seq, 0, 1);
      switch (emp, front) {
        case (#empty, #leaf(x)) ?(x, rest);
        case _ { assert false; loop { }};
      }
    } else {
      null
    }
  };

  public func popBack<X>(seq : Sequence<X>) : ?(Sequence<X>, X) {
    let s = size(seq);
    if (s > 0) {
      let (rest, back, emp) = slice(seq, s - 1 : Nat, 1);
      switch (back, emp) {
        case (#leaf(x), #empty) ?(rest, x);
        case _ { assert false; loop { }};
      }
    } else {
      null
    }
  };

  public func peekBack<X>(seq : Sequence<X>) : ?X {
    if (size(seq) > 0) {
      get(seq, size(seq) - 1 : Nat)
    } else null
  };

  public func peekFront<X>(seq : Sequence<X>) : ?X {
    get(seq, 0)
  };

  public func pushFront<X>(data : X, level : Level, seq : Sequence<X>) : Sequence<X> {
    appendLevel(make(data), level, seq)
  };

  /// Perform [an associative, binary operation](https://en.wikipedia.org/wiki/Monoid#Definition) over the binary tree.
  ///
  /// Like monoid, but simpler (common input and output type).
  public func binaryOp<X>(
    s : Sequence<X>,
    zero : X,
    bop : (X, X) -> X
  ) : X {
    switch s {
      case (#empty) { zero };
      case (#leaf(x)) { x };
      case (#branch(b)) {
             bop( binaryOp(b.left, zero, bop),
                  binaryOp(b.right, zero, bop)
             )
           };
    }
  };

  /// Transform sequence into [monoid structure](https://en.wikipedia.org/wiki/Monoid#Definition)
  ///
  /// The monoid's id element stands in for empty sequences.
  ///
  /// The leaf function maps a leaf element to a monoid element.
  ///
  /// The function binOp gives the monoid's binary operation over elements.
  public func monoid<X, Y>(
    s : Sequence<X>,
    id : Y,
    leaf : X -> Y,
    binOp : (Y, Y) -> Y
  ) : Y {
    switch s {
      case (#empty) { id };
      case (#leaf(x)) { leaf(x) };
      case (#branch(b)) {
             binOp(monoid(b.left, id, leaf, binOp),
                   monoid(b.right, id, leaf, binOp)
             )
           };
    }
  };

  /// Like monoid, except that branch function gets full branch node info
  public func foldUp<X, Y>(
    s : Sequence<X>,
    empty : Y,
    leaf : X -> Y,
    branch : (Branch<X>, Y, Y) -> Y
  ) : Y {
    switch s {
      case (#empty) { empty };
      case (#leaf(x)) { leaf(x) };
      case (#branch(b)) {
             branch( b,
                     foldUp(b.left, empty, leaf, branch),
                     foldUp(b.right, empty, leaf, branch)
             )
           };
    }
  };

  /// Relate child order and iteration order.
  ///
  /// (all defined here, in code)
  public func branchChild<X>(b : Branch<X>, rank : {#fst; #snd}, dir : {#fwd; #bwd}) : Sequence<X> {
    switch (rank, dir) {
    case (#fst, #fwd) { b.left };
    case (#snd, #fwd) { b.right };
    case (#fst, #bwd) { b.right };
    case (#snd, #bwd) { b.left };
    }
  };

  /**
   Fold with directed sequential dependencies.

   Folds the binary tree into an accumulated value, forward or backward.

   Each function is optional, and behaves like the identity function when `null`.

   Branch case accumulates across five steps:
    - two subtrees of the branch, and
    - three points surrounding them (pre, mid, post branch).

   Leaf case accepts an accumulator, initially `empty`.
   */
  public func foldDir<X, Y>(
    s : Sequence<X>,
    dir : {#fwd; #bwd},
    empty : Y,
    leaf : ?((Y, X) -> Y),
    preBranch : ?((Y, Branch<X>) -> Y),
    midBranch : ?((Y, Branch<X>) -> Y),
    postBranch : ?((Y, Branch<X>) -> Y)
  ) : Y {
    switch s {
      case (#empty) { empty };
      case (#leaf(x)) {
             switch leaf {
             case null empty;
             case (?leaf) leaf(empty, x)
             }
           };
      case (#branch(b)) {
             // accumulate in five steps:
             // - two subtrees of the branch, and
             // - three points surrounding them (pre, mid, post branch).
             let a1 = switch (preBranch, dir, postBranch) {
                       case (null, #fwd, _) empty;
                       case (_, #bwd, null) empty;
                       case (?pb, #fwd, _) pb(empty, b);
                       case (_, #bwd, ?pb) pb(empty, b);
             };
             let a2 = foldDir(branchChild(b, #fst, dir),
                              dir, a1, leaf,
                              preBranch, midBranch, postBranch);
             let a3 = switch midBranch {
               case null a2;
               case (?mb) mb(a2, b);
             };
             let a4 = foldDir(branchChild(b, #snd, dir),
                              dir, a3, leaf,
                              preBranch, midBranch, postBranch);
             let a5 = switch (preBranch, dir, postBranch) {
                       case (null, #bwd, _) empty;
                       case (_, #fwd, null) empty;
                       case (?pb, #bwd, _) pb(a4, b);
                       case (_, #fwd, ?pb) pb(a4, b);
             };
             a5
           };
    }
  };

  type IterRep<X> = List.List<Sequence<X>>;

  public func iter<X>(s : Sequence<X>, dir : {#fwd; #bwd}) : Iter.Iter<X> {
    object {
      var seqs : IterRep<X> = ?(s, null);
      public func next() : ?X {
        switch (dir, seqs) {
        case (_, null) { null };
        case (_, ?(#empty, ts))        { seqs := ts; next() };
        case (_, ?(#leaf(x), ts))      { seqs := ts; ?x };
        case (#fwd, ?(#branch(b), ts)) { seqs := ?(b.left, ?(b.right, ts)); next() };
        case (#bwd, ?(#branch(b), ts)) { seqs := ?(b.right, ?(b.left, ts)); next() };
        }
      }
    }
  };

  public func fromAssocList<K, V, T>(
    l : List.List<(Trie.Key<K>, V)>,
    kv : (Trie.Key<K>, V) -> T) : Sequence<T> {
    switch l {
      case null { #empty };
      case (?(h,t)) { branch<T>(make<T>(kv h), 0, fromAssocList(t, kv)) };
    }
  };

  public func fromTrie<K, V, T>(
    t : Trie.Trie<K, V>,
    kv : (Trie.Key<K>, V) -> T) : Sequence<T> {
    switch t {
    case (#empty) #empty;
    case (#leaf(leaf)) fromAssocList(leaf.keyvals, kv);
    case (#branch(b)) {
           // size (of original sub-trie) can serve as a valid level
           branch<T>(fromTrie(b.left, kv),
                     Nat32.fromNat(b.size),
                     fromTrie(b.right, kv))
         }
    }
  };

  /// separate into (nested) sub-sequences by looking for sub-sequence delimiter elements (omitted from output)
  public func tokens<X>(s : Sequence<X>, isDelim : X -> Bool, levels : Stream<Level>) : Sequence<Sequence<X>> {
    var outer : Sequence<Sequence<X>> = empty();
    var inner : Sequence<X> = empty();
    for (x in iter(s, #fwd)) {
      if (isDelim(x)) {
        outer := pushBack(outer, levels.next(), inner);
        inner := empty();
      }
      else {
        inner := pushBack(inner, levels.next(), x)
      }
    };
    if (size(inner) > 0) {
      outer := pushBack(outer, levels.next(), inner)
    };
    outer
  };

}
