/// Functional set
///
/// Sets are partial maps from element type to unit type,
/// i.e., the partial map represents the set with its domain.

// TODO-Matthew:
// ---------------
//
// - for now, we pass a hash value each time we pass an element value;
//   in the future, we might avoid passing element hashes with each element in the API;
//   related to: https://dfinity.atlassian.net/browse/AST-32
//
// - similarly, we pass an equality function when we do some operations.
//   in the future, we might avoid this via https://dfinity.atlassian.net/browse/AST-32
import Trie "Trie";
import Hash "Hash";
import List "List";

module {

  public type Hash = Hash.Hash;
  public type Set<T> = Trie.Trie<T,()>;

  /// Empty set.
  public func empty<T>() : Set<T> { Trie.empty<T,()>(); };

  /// Put an element into the set.
  public func put<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T> {
    let (s2, _) = Trie.put<T,()>(s, { key = x; hash = xh }, eq, ());
    s2
  };

  /// Delete an element from the set.
  public func delete<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T> {
    let (s2, _) = Trie.remove<T, ()>(s, { key = x; hash = xh }, eq);
    s2
  };

  /// Test if two sets are equal.
  public func equal<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Bool {
    // XXX: Todo: use a smarter check
    func unitEqual (_ : (),_ : ()) : Bool { true };
    Trie.equalStructure<T, ()>(s1, s2, eq, unitEqual)
  };

  /// The number of set elements, set's cardinality.
  public func size<T>(s : Set<T>) : Nat {
    Trie.foldUp<T, (), Nat>(
      s,
      func(n : Nat, m : Nat) : Nat { n + m },
      func(_ : T, _ : ()) : Nat { 1 },
      0
    )
  };

  /// Test if a set contains a given element.
  public func mem<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Bool {
    switch (Trie.find<T, ()>(s, { key = x; hash = xh }, eq)) {
      case null { false };
      case (?_) { true };
    }
  };

  /// [Set union](https://en.wikipedia.org/wiki/Union_(set_theory)).
  public func union<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T> {
    let s3 = Trie.merge<T, ()>(s1, s2, eq);
    s3
  };

  /// [Set difference](https://en.wikipedia.org/wiki/Difference_(set_theory)).
  public func diff<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T> {
    let s3 = Trie.diff<T, (), ()>(s1, s2, eq);
    s3
  };

  /// [Set intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory)).
  public func intersect<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T> {
    let noop : ((), ()) -> (()) = func (_ : (), _ : ()) : (()) = ();
    let s3 = Trie.join<T, (), (), ()>(s1, s2, eq, noop);
    s3
  };

  //// Construct a set from an array.
  public func fromArray<T>(arr : [T], elemHash : T -> Hash, eq : (T, T) -> Bool) : Set<T> {
    var s = empty<T>();
    for (elem in arr.vals()) {
      s := put<T>(s, elem, elemHash(elem), eq);
    };
    s
  };

  //// Returns the set as an array.
  public func toArray<T>(s : Set<T>): [T] {
    Trie.toArray(s, func (t : T, _ : ()) : T { t })
  }

}
