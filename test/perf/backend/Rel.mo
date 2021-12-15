import Trie "mo:base/Trie";
import List "mo:base/List";
import Iter "mo:base/Iter";
import Hash "mo:base/Hash";
import Text "mo:base/Text";
import Prelude "mo:base/Prelude";

/// Binary relation representation.
///
/// https://en.wikipedia.org/wiki/Binary_relation
///
/// Properties of this implementation:
///
/// - Uses (purely functional) tries from base library.
/// - Each operation is fast (sublinear, O(log n) time).
/// - Relations permit cheap O(1)-time copies; versioned history is possible.
///
/// Use this representation to implement binary relations (e.g.,
/// CanCan videos and CanCan users) that can be represented, merged
/// and analyzed separately from the data that they relate.
///
/// The goal of this representation is to isolate common patterns for
/// relations, and reduce the boilerplate of the alternative (bespoke
/// system) design, where each kind of thing has internal collections
/// (arrays or lists or maps) of the things to which it is related.
/// That representation can be reconstituted as a view of this one.
///
module {
  public type HashPair<X, Y> =
    ( X -> Hash.Hash,
      Y -> Hash.Hash );

  public type EqualPair<X, Y> =
    ( (X, X) -> Bool,
      (Y, Y) -> Bool) ;

  /// Relation between X's and Y's.
  ///
  /// Uses two (related) hash tries, for the edges in each direction.
  /// Holds the hash and equal functions for the tries.
  public type Rel<X, Y> = {
    forw : Trie.Trie2D<X, Y, ()> ;
    back : Trie.Trie2D<Y, X, ()> ;
    hash : HashPair<X, Y> ;
    equal : EqualPair<X, Y> ;
  };

  /// Relation between X's and Y's.
  ///
  /// Shared type (no hash or equal functions).
  public type RelShared<X, Y> = {
    forw : Trie.Trie2D<X, Y, ()> ;
    //
    // No HO functions, and no backward direction:
    // In a serialized message form, the backward direction is redundant
    // and can be recomputed in linear time from the forw field.
    //
    // back : Trie.Trie2D<Y, X, ()> ;
  };

  public func share<X, Y>( rel : Rel<X, Y> ) : RelShared<X, Y> {
    { forw = rel.forw ;
      // back = rel.back ;
    }
  };

  public func fromShare<X, Y>( rel : RelShared<X, Y>,
                               hash_ : HashPair<X, Y>,
                               equal_ : EqualPair<X, Y> ) : Rel<X, Y>
  {
    { forw = rel.forw ;
      back = invert(rel.forw);
      hash = hash_ ;
      equal = equal_
    }
  };

  public func keyOf0<X, Y>( rel : Rel<X, Y>,  x : X) : Trie.Key<X> {
    { key = x ; hash = rel.hash.0(x) }
  };

  public func keyOf1<X, Y>( rel : Rel<X, Y>,  y : Y) : Trie.Key<Y> {
    { key = y ; hash = rel.hash.1(y) }
  };

  public func keyOf<X, Y>( rel : Rel<X, Y>, p : (X, Y))
    : (Trie.Key<X>, Trie.Key<Y>)
  {
    (keyOf0(rel, p.0),
     keyOf1(rel, p.1))
  };

  public func empty<X, Y>( hash_ : HashPair<X, Y>,
                           equal_ : EqualPair<X, Y>) : Rel<X, Y> {
    {
      forw = Trie.empty();
      back = Trie.empty();
      hash = hash_ ;
      equal = equal_
    }
  };

  public func isMember<X, Y>(rel : Rel<X, Y>, x : X, y : Y) : Bool {
    switch (Trie.find<X, Trie.Trie<Y, ()>>(rel.forw, keyOf0(rel, x), rel.equal.0)) {
    case null false;
    case (?t) {
           switch (Trie.find<Y, ()>(t, keyOf1(rel, y), rel.equal.1)) {
           case null false;
           case _ true;
           }
         };
    }
  };

  public func getRelated0<X, Y>(rel : Rel<X, Y>, x : X) : Iter.Iter<Y> {
    let t = Trie.find<X, Trie.Trie<Y, ()>>(rel.forw, keyOf0(rel, x), rel.equal.0);
    switch t {
      // to do -- define as Iter.empty()
      case null { object { public func next() : ?Y { null } } };
      case (?t) { iterAll(t) };
    }
  };

  public func getRelated1<X, Y>(rel : Rel<X, Y>, y : Y) : Iter.Iter<X> {
    let t = Trie.find(rel.back, keyOf1(rel, y), rel.equal.1);
    switch t {
      case null { object { public func next() : ?X { null } } };
      case (?t) { iterAll(t) };
    }
  };

  public func put<X, Y>( rel : Rel<X, Y>, p : (X, Y)) : Rel<X, Y> {
    let k = keyOf(rel, p);
    {
      forw = Trie.put2D(rel.forw, k.0, rel.equal.0, k.1, rel.equal.1, ()) ;
      back = Trie.put2D(rel.back, k.1, rel.equal.1, k.0, rel.equal.0, ()) ;
      hash = rel.hash ;
      equal = rel.equal ;
    }
  };

  public func delete<X, Y>( rel : Rel<X, Y>, p : (X, Y)) : Rel<X, Y> {
    let k = (keyOf0(rel, p.0), keyOf1(rel, p.1));
    {
      forw = Trie.remove2D(rel.forw, k.0, rel.equal.0, k.1, rel.equal.1).0 ;
      back = Trie.remove2D(rel.back, k.1, rel.equal.1, k.0, rel.equal.0).0 ;
      hash = rel.hash ;
      equal = rel.equal ;
    }
  };

  func invert<X, Y>(rel : Trie.Trie2D<X, Y, ()>) : Trie.Trie2D<Y, X, ()> {
    Prelude.nyi() // to do -- for testing / upgrades sub-story
  };

  // helper for getRelated{0,1}
  func iterAll<K>(t : Trie.Trie<K, ()>)
    : Iter.Iter<K>
    =
    object {
    var stack = ?(t, null) : List.List<Trie.Trie<K, ()>>;
    public func next() : ?K {
      switch stack {
      case null { null };
      case (?(trie, stack2)) {
             switch trie {
             case (#empty) {
                    stack := stack2;
                    next()
                  };
             case (#leaf({keyvals=null})) {
                    stack := stack2;
                    next()
                  };
             case (#leaf({size=c; keyvals=?((k2, _), kvs)})) {
                    stack := ?(#leaf({size=c-1; keyvals=kvs}), stack2);
                    ?k2.key
                  };
             case (#branch(br)) {
                    stack := ?(br.left, ?(br.right, stack2));
                    next()
                  };
             }
           }
      }
    }
  };


}
