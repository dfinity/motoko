/// Pure (immutable) sets based on order/comparison of elements.
/// A set is a collection of elements without duplicates.
/// The set data structure type is stable and can be used for orthogonal persistence.
///
/// Example:
/// ```motoko
/// import Set "mo:core/pure/Set";
/// import Nat "mo:core/Nat";
///
/// persistent actor {
///   let set = Set.fromIter([3, 1, 2, 3].values(), Nat.compare);
///   assert Set.size(set) == 3;
///   assert not Set.contains(set, Nat.compare, 4);
///   let diff = Set.difference(set, set, Nat.compare);
///   assert Set.isEmpty(diff);
/// }
/// ```
///
/// These sets are implemented as red-black trees, a balanced binary search tree of ordered elements.
///
/// The tree data structure internally colors each of its nodes either red or black,
/// and uses this information to balance the tree during modifying operations.
///
/// Performance:
/// * Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
/// * Space: `O(n)` for storing the entire tree.
/// `n` denotes the number of elements (i.e. nodes) stored in the tree.
///
/// Credits:
///
/// The core of this implementation is derived from:
///
/// * Ken Friis Larsen's [RedBlackMap.sml](https://github.com/kfl/mosml/blob/master/src/mosmllib/Redblackmap.sml), which itself is based on:
/// * Stefan Kahrs, "Red-black trees with types", Journal of Functional Programming, 11(4): 425-432 (2001), [version 1 in web appendix](http://www.cs.ukc.ac.uk/people/staff/smk/redblack/rb.html).

import Runtime "../Runtime";
import List "../imperative/List"; // NB: imperative!
import Iter "../imperative/Iter";
import Types "../Types";
import Nat "../Nat";
import Order "../Order";

module {

  /// Ordered collection of unique elements of the generic type `T`.
  /// If type `T` is stable then `Set<T>` is also stable.
  /// To ensure that property the `Set<T>` does not have any methods,
  /// instead they are gathered in the functor-like class `Operations` (see example there).
  public type Set<T> = Types.Pure.Set<T>;

  /// Red-black tree of nodes with ordered set elements.
  /// Leaves are considered implicitly black.
  type Tree<T> = Types.Pure.Set.Tree<T>;

  /// Create a set with the elements obtained from an iterator.
  /// Potential duplicate elements in the iterator are ignored, i.e.
  /// multiple occurrences of an equal element only occur once in the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([3, 1, 2, 1].values(), Nat.compare);
  ///   assert Iter.toArray(Set.values(set)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements returned by the iterator and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func fromIter<T>(iter : Iter.Iter<T>, compare : (T, T) -> Order.Order) : Set<T> {
    var set = empty() : Set<T>;
    for (val in iter) {
      set := Internal.add(set, compare, val)
    };
    set
  };

  /// Given a `set` ordered by `compare`, insert the new `element`,
  /// returning the new set.
  ///
  /// Return the set unchanged if the element already exists in the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set0 = Set.empty<Nat>();
  ///   let set1 = Set.add(set0, Nat.compare, 2);
  ///   let set2 = Set.add(set1, Nat.compare, 1);
  ///   let set3 = Set.add(set2, Nat.compare, 2);
  ///   assert Iter.toArray(Set.values(set0)) == [];
  ///   assert Iter.toArray(Set.values(set1)) == [2];
  ///   assert Iter.toArray(Set.values(set2)) == [1, 2];
  ///   assert Iter.toArray(Set.values(set3)) == [1, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned set shares with the `set` most of the tree nodes.
  /// Garbage collecting one of the sets (e.g. after an assignment `m := Set.add(m, c, e)`)
  /// causes collecting `O(log(n))` nodes.
  public func add<T>(set : Set<T>, compare : (T, T) -> Order.Order, elem : T) : Set<T> = Internal.add(set, compare, elem);

  /// Given `set` ordered by `compare`, insert the new `element`,
  /// returning the set extended with `element` and a Boolean indicating
  /// if the element was already present in `set`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set0 = Set.empty();
  ///   do {
  ///     let (set1, new1) = Set.insert(set0, Nat.compare, 2);
  ///     assert new1;
  ///     let (set2, new2) = Set.insert(set1, Nat.compare, 1);
  ///     assert new2;
  ///     let (set3, new3) = Set.insert(set2, Nat.compare, 2);
  ///     assert not new3;
  ///     assert Iter.toArray(Set.values(set3)) == [1, 2]
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned set shares with the `set` most of the tree nodes.
  /// Garbage collecting one of the sets (e.g. after an assignment `m := Set.add(m, c, e)`)
  /// causes collecting `O(log(n))` nodes.
  public func insert<T>(set : Set<T>, compare : (T, T) -> Order.Order, elem : T) : (Set<T>, Bool) = Internal.insert<T>(set, compare, elem);

  /// Given `set` ordered by `compare` return the set with `element` removed.
  /// Return the set unchanged if the element was absent.
  ///
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///
  ///   let set1 = Set.remove(set, Nat.compare, 2);
  ///   let set2 = Set.remove(set1, Nat.compare, 4);
  ///   assert Iter.toArray(Set.values(set2)) == [1, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` including garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(log(n))` objects that will be collected as garbage.
  /// Note: The returned set shares with `set` most of the tree nodes.
  /// Garbage collecting one of the sets (e.g. after an assignment `m := Set.delete(m, c, e)`)
  /// causes collecting `O(log(n))` nodes.
  public func remove<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Set<T> = Internal.remove(set, compare, element);

  /// Given `set` ordered by `compare`, delete `element` from the set, returning
  /// either the set without the element and a Boolean indicating whether
  /// whether `element` was contained in `set`.
  ///
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   do {
  ///     let (set1, contained1) = Set.delete(set, Nat.compare, 2);
  ///     assert contained1;
  ///     assert Iter.toArray(Set.values(set1)) == [1, 3];
  ///     let (set2, contained2) = Set.delete(set1, Nat.compare, 4);
  ///     assert not contained2;
  ///     assert Iter.toArray(Set.values(set2)) == [1, 3];
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` including garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(log(n))` objects that will be collected as garbage.
  /// Note: The returned set shares with `set` most of the tree nodes.
  /// Garbage collecting one of the sets (e.g. after an assignment `m := Set.delete(m, c, e)`)
  /// causes collecting `O(log(n))` nodes.
  public func delete<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : (Set<T>, Bool) = Internal.delete(set, compare, element);

  /// Tests whether the set contains the provided element.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Bool "mo:core/Bool";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([3, 1, 2].values(), Nat.compare);
  ///
  ///   assert Set.contains(set, Nat.compare, 1);
  ///   assert not Set.contains(set, Nat.compare, 4);
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func contains<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Bool = Internal.contains(set.root, compare, element);

  /// Get the maximal element of the set `set` if it is not empty, otherwise returns `null`
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter<Nat>([0, 2, 1].values(), Nat.compare);
  ///   let set2 = Set.empty<Nat>();
  ///   assert Set.max(set1) == ?2;
  ///   assert Set.max(set2) == null;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements in the set
  public func max<T>(s : Set<T>) : ?T = Internal.max(s.root);

  /// Retrieves the minimum element from the set.
  /// If the set is empty, returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter<Nat>([2, 0, 1].values(), Nat.compare);
  ///   let set2 = Set.empty<Nat>();
  ///   assert Set.min(set1) == ?0;
  ///   assert Set.min(set2) == null;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the set.
  public func min<T>(s : Set<T>) : ?T = Internal.min(s.root);

  /// Returns a new set that is the union of `set1` and `set2`,
  /// i.e. a new set that all the elements that exist in at least on of the two sets.
  /// Potential duplicates are ignored, i.e. if the same element occurs in both `set1`
  /// and `set2`, it only occurs once in the returned set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  ///   let union = Set.union(set1, set2, Nat.compare);
  ///   assert Iter.toArray(Set.values(union)) == [1, 2, 3, 4, 5];
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(m)`, retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements in the sets, and `m <= n`.
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.
  public func union<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    if (size(set1) < size(set2)) {
      foldLeft(set1, set2, func(acc : Set<T>, elem : T) : Set<T> { Internal.add(acc, compare, elem) })
    } else {
      foldLeft(set2, set1, func(acc : Set<T>, elem : T) : Set<T> { Internal.add(acc, compare, elem) })
    }
  };

  /// Returns a new set that is the intersection of `set1` and `set2`,
  /// i.e. a new set that contains all the elements that exist in both sets.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([0, 1, 2].values(), Nat.compare);
  ///   let set2 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let intersection = Set.intersection(set1, set2, Nat.compare);
  ///   assert Iter.toArray(Set.values(intersection)) == [1, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(m)` temporary objects that will be collected as garbage.
  public func intersection<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    let elems = List.empty<T>();
    if (set1.size < set2.size) {
      Internal.iterate(
        set1.root,
        func(x : T) {
          if (Internal.contains(set2.root, compare, x)) {
            List.add(elems, x)
          }
        }
      )
    } else {
      Internal.iterate(
        set2.root,
        func(x : T) {
          if (Internal.contains(set1.root, compare, x)) {
            List.add(elems, x)
          }
        }
      )
    };
    { root = Internal.buildFromSorted(elems); size = List.size(elems) }
  };

  /// Returns a new set that is the difference between `set1` and `set2` (`set1` minus `set2`),
  /// i.e. a new set that contains all the elements of `set1` that do not exist in `set2`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  ///   let difference = Set.difference(set1, set2, Nat.compare);
  ///   assert Iter.toArray(Set.values(difference)) == [1, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(m * log(n))` temporary objects that will be collected as garbage.
  public func difference<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    if (size(set1) < size(set2)) {
      let elems = List.empty<T>(); /* imperative! */
      Internal.iterate(
        set1.root,
        func(x : T) {
          if (not Internal.contains(set2.root, compare, x)) {
            List.add(elems, x)
          }
        }
      );
      { root = Internal.buildFromSorted(elems); size = List.size(elems) }
    } else {
      foldLeft(
        set2,
        set1,
        func(acc : Set<T>, elem : T) : Set<T> {
          if (Internal.contains(acc.root, compare, elem)) {
            Internal.remove(acc, compare, elem)
          } else { acc }
        }
      )
    }
  };

  /// Project all elements of the set in a new set.
  /// Apply a mapping function to each element in the set and
  /// collect the mapped elements in a new mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Text "mo:core/Text";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let numbers = Set.fromIter([3, 1, 2].values(), Nat.compare);
  ///
  ///   let textNumbers =
  ///     Set.map<Nat, Text>(numbers, Text.compare, Nat.toText);
  ///   assert Iter.toArray(Set.values(textNumbers)) == ["1", "2", "3"];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func map<T1, T2>(s : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> T2) : Set<T2> = Internal.foldLeft(s.root, empty<T2>(), func(acc : Set<T2>, elem : T1) : Set<T2> { Internal.add(acc, compare, project(elem)) });

  /// Apply an operation on each element contained in the set.
  /// The operation is applied in ascending order of the elements.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let numbers = Set.fromIter([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   var text = "";
  ///   Set.forEach<Nat>(numbers, func (element) {
  ///     text #= " " # Nat.toText(element)
  ///   });
  ///   assert text == " 0 1 2 3";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory.
  /// where `n` denotes the number of elements stored in the set.
  ///
  public func forEach<T>(set : Set<T>, operation : T -> ()) {
    ignore foldLeft<T, Null>(set, null, func(acc, e) : Null { operation(e); null })
  };

  /// Filter elements in a new set.
  /// Create a copy of the mutable set that only contains the elements
  /// that fulfil the criterion function.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let numbers = Set.fromIter([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   let evenNumbers = Set.filter<Nat>(numbers, Nat.compare, func (number) {
  ///     number % 2 == 0
  ///   });
  ///   assert Iter.toArray(Set.values(evenNumbers)) == [0, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func filter<T>(set : Set<T>, compare : (T, T) -> Order.Order, criterion : T -> Bool) : Set<T> {
    foldLeft<T, Set<T>>(
      set,
      empty(),
      func(acc, e) {
        if (criterion(e)) (add(acc, compare, e)) else acc
      }
    )
  };

  /// Filter all elements in the set by also applying a projection to the elements.
  /// Apply a mapping function `project` to all elements in the set and collect all
  /// elements, for which the function returns a non-null new element. Collect all
  /// non-discarded new elements in a new mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Text "mo:core/Text";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let numbers = Set.fromIter([3, 0, 2, 1].values(), Nat.compare);
  ///
  ///   let evenTextNumbers = Set.filterMap<Nat, Text>(numbers, Text.compare, func (number) {
  ///     if (number % 2 == 0) {
  ///        ?Nat.toText(number)
  ///     } else {
  ///        null // discard odd numbers
  ///     }
  ///   });
  ///   assert Iter.toArray(Set.values(evenTextNumbers)) == ["0", "2"];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func filterMap<T1, T2>(set : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> ?T2) : Set<T2> {
    func combine(acc : Set<T2>, elem : T1) : Set<T2> {
      switch (project(elem)) {
        case null { acc };
        case (?elem2) {
          Internal.add(acc, compare, elem2)
        }
      }
    };
    Internal.foldLeft(set.root, empty(), combine)
  };

  /// Test whether `set1` is a sub-set of `set2`, i.e. each element in `set1` is
  /// also contained in `set2`. Returns `true` if both sets are equal.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([1, 2].values(), Nat.compare);
  ///   let set2 = Set.fromIter([2, 1, 0].values(), Nat.compare);
  ///   let set3 = Set.fromIter([3, 4].values(), Nat.compare);
  ///   assert Set.isSubset(set1, set2, Nat.compare);
  ///   assert not Set.isSubset(set1, set3, Nat.compare);
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func isSubset<T>(s1 : Set<T>, s2 : Set<T>, compare : (T, T) -> Order.Order) : Bool {
    if (s1.size > s2.size) { return false };
    isSubsetHelper(s1.root, s2.root, compare)
  };

  /// Test whether two sets are equal.
  /// Both sets have to be constructed by the same comparison function.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([1, 2].values(), Nat.compare);
  ///   let set2 = Set.fromIter([2, 1].values(), Nat.compare);
  ///   let set3 = Set.fromIter([2, 1, 0].values(), Nat.compare);
  ///   assert Set.equal(set1, set2, Nat.compare);
  ///   assert not Set.equal(set1, set3, Nat.compare);
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements stored in the sets set1 and set2, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func equal<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Bool {
    if (set1.size != set2.size) { return false };
    isSubsetHelper(set1.root, set2.root, compare)
  };

  func isSubsetHelper<T>(t1 : Tree<T>, t2 : Tree<T>, compare : (T, T) -> Order.Order) : Bool {
    switch (t1, t2) {
      case (#leaf, _) { true };
      case (_, #leaf) { false };
      case ((#red(t1l, x1, t1r) or #black(t1l, x1, t1r)), (#red(t2l, x2, t2r)) or #black(t2l, x2, t2r)) {
        switch (compare(x1, x2)) {
          case (#equal) {
            isSubsetHelper(t1l, t2l, compare) and isSubsetHelper(t1r, t2r, compare)
          };
          // x1 < x2 ==> x1 \in t2l /\ t1l \subset t2l
          case (#less) {
            Internal.contains(t2l, compare, x1) and isSubsetHelper(t1l, t2l, compare) and isSubsetHelper(t1r, t2, compare)
          };
          // x2 < x1 ==> x1 \in t2r /\ t1r \subset t2r
          case (#greater) {
            Internal.contains(t2r, compare, x1) and isSubsetHelper(t1l, t2, compare) and isSubsetHelper(t1r, t2r, compare)
          }
        }
      }
    }
  };

  /// Compare two sets by comparing the elements.
  /// Both sets must have been created by the same comparison function.
  /// The two sets are iterated by the ascending order of their creation and
  /// order is determined by the following rules:
  /// Less:
  /// `set1` is less than `set2` if:
  ///  * the pairwise iteration hits an element pair `element1` and `element2` where
  ///    `element1` is less than `element2` and all preceding elements are equal, or,
  ///  * `set1` is  a strict prefix of `set2`, i.e. `set2` has more elements than `set1`
  ///     and all elements of `set1` occur at the beginning of iteration `set2`.
  /// Equal:
  /// `set1` and `set2` have same series of equal elements by pairwise iteration.
  /// Greater:
  /// `set1` is neither less nor equal `set2`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([0, 1].values(), Nat.compare);
  ///   let set2 = Set.fromIter([0, 2].values(), Nat.compare);
  ///
  ///   assert Set.compare(set1, set2, Nat.compare) == #less;
  ///   assert Set.compare(set1, set1, Nat.compare) == #equal;
  ///   assert Set.compare(set2, set1, Nat.compare) == #greater;
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that `compare` has runtime and space costs of `O(1)`.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func compare<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Order.Order {
    // TODO: optimize using recursion on set1?
    let iterator1 = values(set1);
    let iterator2 = values(set2);
    loop {
      switch (iterator1.next(), iterator2.next()) {
        case (null, null) return #equal;
        case (null, _) return #less;
        case (_, null) return #greater;
        case (?element1, ?element2) {
          let comparison = compare(element1, element2);
          if (comparison != #equal) {
            return comparison
          }
        }
      }
    }
  };

  /// Returns an iterator over the elements in the set,
  /// traversing the elements in the ascending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 2, 3, 1].values(), Nat.compare);
  ///
  ///   var text = "";
  ///   for (number in Set.values(set)) {
  ///      text #= " " # Nat.toText(number);
  ///   };
  ///   assert text == " 0 1 2 3";
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func values<T>(set : Set<T>) : Iter.Iter<T> = Internal.iter(set.root, #fwd);

  /// Returns an iterator over the elements in the set,
  /// traversing the elements in the descending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 2, 3, 1].values(), Nat.compare);
  ///
  ///   var tmp = "";
  ///   for (number in Set.reverseValues(set)) {
  ///      tmp #= " " # Nat.toText(number);
  ///   };
  ///   assert tmp == " 3 2 1 0";
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func reverseValues<T>(set : Set<T>) : Iter.Iter<T> = Internal.iter(set.root, #bwd);

  /// Create a new empty set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   assert Iter.toArray(Set.values(set)) == [];
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<T>() : Set<T> = { root = #leaf; size = 0 };

  /// Create a new set with a single element.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.singleton(0);
  ///   assert Iter.toArray(Set.values(set)) == [0];
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func singleton<T>(element : T) : Set<T> {
    {
      size = 1;
      root = #red(#leaf, element, #leaf)
    }
  };

  /// Return the number of elements in a set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 3, 2, 1, 3].values(), Nat.compare);
  ///
  ///   assert Set.size(set) == 4;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func size<T>(set : Set<T>) : Nat = set.size;

  /// Iterate all elements in ascending order,
  /// and accumulate the elements by applying the combine function, starting from a base value.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 3, 2, 1].values(), Nat.compare);
  ///
  ///   let text = Set.foldLeft<Nat, Text>(
  ///      set,
  ///      "",
  ///      func (accumulator, element) {
  ///        accumulator # " " # Nat.toText(element)
  ///      }
  ///   );
  ///   assert text == " 0 1 2 3";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  public func foldLeft<T, A>(
    set : Set<T>,
    base : A,
    combine : (A, T) -> A
  ) : A = Internal.foldLeft(set.root, base, combine);

  /// Iterate all elements in descending order,
  /// and accumulate the elements by applying the combine function, starting from a base value.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 3, 2, 1].values(), Nat.compare);
  ///
  ///   let text = Set.foldRight<Nat, Text>(
  ///      set,
  ///      "",
  ///      func (element, accumulator) {
  ///         accumulator # " " # Nat.toText(element)
  ///      }
  ///   );
  ///   assert text == " 3 2 1 0";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  public func foldRight<T, A>(
    set : Set<T>,
    base : A,
    combine : (T, A) -> A
  ) : A = Internal.foldRight(set.root, base, combine);

  /// Determines whether a set is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set1 = Set.empty<Nat>();
  ///   let set2 = Set.singleton<Nat>(1);
  ///
  ///   assert Set.isEmpty(set1);
  ///   assert not Set.isEmpty(set2);
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func isEmpty<T>(set : Set<T>) : Bool {
    switch (set.root) {
      case (#leaf) { true };
      case _ { false }
    }
  };

  /// Check whether all element in the set satisfy a predicate, i.e.
  /// the `predicate` function returns `true` for all elements in the set.
  /// Returns `true` for an empty set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   let belowTen = Set.all<Nat>(set, func (number) {
  ///     number < 10
  ///   });
  ///   assert belowTen;
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the set.
  public func all<T>(set : Set<T>, predicate : T -> Bool) : Bool = Internal.all(set.root, predicate);

  /// Check whether at least one element in the set satisfies a predicate, i.e.
  /// the `predicate` function returns `true` for at least one element in the set.
  /// Returns `false` for an empty set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   let aboveTen = Set.any<Nat>(set, func (number) {
  ///     number > 10
  ///   });
  ///   assert not aboveTen;
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func any<T>(s : Set<T>, pred : T -> Bool) : Bool = Internal.any(s.root, pred);

  /// Test helper that check internal invariant for the given set `s`.
  /// Raise an error (for a stack trace) if invariants are violated.
  public func assertValid<T>(set : Set<T>, compare : (T, T) -> Order.Order) : () {
    Internal.assertValid(set, compare)
  };

  /// Generate a textual representation of all the elements in the set.
  /// Primarily to be used for testing and debugging.
  /// The elements are formatted according to `elementFormat`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   assert Set.toText(set, Nat.toText) == "PureSet{0, 1, 2, 3}";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that `elementFormat` has runtime and space costs of `O(1)`.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func toText<T>(set : Set<T>, elementFormat : T -> Text) : Text {
    var text = "PureSet{";
    var sep = "";
    for (element in values(set)) {
      text #= sep # elementFormat(element);
      sep := ", "
    };
    text # "}"
  };

  /// Construct the union of a set of element sets, i.e. all elements of
  /// each element set are included in the result set.
  /// Any duplicates are ignored, i.e. if the same element occurs in multiple element sets,
  /// it only occurs once in the result set.
  ///
  /// Assumes all sets are ordered by `compare`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Order "mo:core/Order";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   func setCompare(first: Set.Set<Nat>, second: Set.Set<Nat>) : Order.Order {
  ///      Set.compare(first, second, Nat.compare)
  ///   };
  ///
  ///   let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  ///   let set3 = Set.fromIter([5, 6, 7].values(), Nat.compare);
  ///   let setOfSets = Set.fromIter([set1, set2, set3].values(), setCompare);
  ///   let flatSet = Set.flatten(setOfSets, Nat.compare);
  ///   assert Iter.toArray(Set.values(flatSet)) == [1, 2, 3, 4, 5, 6, 7];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of elements stored in all the sub-sets,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func flatten<T>(setOfSets : Set<Set<T>>, compare : (T, T) -> Order.Order) : Set<T> {
    var result = empty<T>();
    for (set in values(setOfSets)) {
      result := union(result, set, compare)
    };
    result
  };

  /// Construct the union of a series of sets, i.e. all elements of
  /// each set are included in the result set.
  /// Any duplicates are ignored, i.e. if an element occurs
  /// in several of the iterated sets, it only occurs once in the result set.
  ///
  /// Assumes all sets are ordered by `compare`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set1 = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let set2 = Set.fromIter([3, 4, 5].values(), Nat.compare);
  ///   let set3 = Set.fromIter([5, 6, 7].values(), Nat.compare);
  ///   let combined = Set.join([set1, set2, set3].values(), Nat.compare);
  ///   assert Iter.toArray(Set.values(combined)) == [1, 2, 3, 4, 5, 6, 7];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of elements stored in the iterated sets,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func join<T>(setIterator : Iter.Iter<Set<T>>, compare : (T, T) -> Order.Order) : Set<T> {
    var result = empty<T>();
    for (set in setIterator) {
      result := union(result, set, compare)
    };
    result
  };

  module Internal {
    public func contains<T>(tree : Tree<T>, compare : (T, T) -> Order.Order, elem : T) : Bool {
      func f(t : Tree<T>, x : T) : Bool {
        switch t {
          case (#black(l, x1, r)) {
            switch (compare(x, x1)) {
              case (#less) { f(l, x) };
              case (#equal) { true };
              case (#greater) { f(r, x) }
            }
          };
          case (#red(l, x1, r)) {
            switch (compare(x, x1)) {
              case (#less) { f(l, x) };
              case (#equal) { true };
              case (#greater) { f(r, x) }
            }
          };
          case (#leaf) { false }
        }
      };
      f(tree, elem)
    };

    public func max<V>(m : Tree<V>) : ?V {
      func rightmost(m : Tree<V>) : V {
        switch m {
          case (#red(_, v, #leaf)) { v };
          case (#red(_, _, r)) { rightmost(r) };
          case (#black(_, v, #leaf)) { v };
          case (#black(_, _, r)) { rightmost(r) };
          case (#leaf) { Runtime.trap "pure/Set.max() impossible" }
        }
      };
      switch m {
        case (#leaf) { null };
        case (_) { ?rightmost(m) }
      }
    };

    public func min<V>(m : Tree<V>) : ?V {
      func leftmost(m : Tree<V>) : V {
        switch m {
          case (#red(#leaf, v, _)) { v };
          case (#red(l, _, _)) { leftmost(l) };
          case (#black(#leaf, v, _)) { v };
          case (#black(l, _, _)) { leftmost(l) };
          case (#leaf) { Runtime.trap "pure/Set.min() impossible" }
        }
      };
      switch m {
        case (#leaf) { null };
        case (_) { ?leftmost(m) }
      }
    };

    public func all<V>(m : Tree<V>, pred : V -> Bool) : Bool {
      switch m {
        case (#red(l, v, r)) {
          pred(v) and all(l, pred) and all(r, pred)
        };
        case (#black(l, v, r)) {
          pred(v) and all(l, pred) and all(r, pred)
        };
        case (#leaf) { true }
      }
    };

    public func any<V>(m : Tree<V>, pred : V -> Bool) : Bool {
      switch m {
        case (#red(l, v, r)) {
          pred(v) or any(l, pred) or any(r, pred)
        };
        case (#black(l, v, r)) {
          pred(v) or any(l, pred) or any(r, pred)
        };
        case (#leaf) { false }
      }
    };

    public func iterate<V>(m : Tree<V>, f : V -> ()) {
      switch m {
        case (#leaf) {};
        case (#black(l, v, r)) { iterate(l, f); f(v); iterate(r, f) };
        case (#red(l, v, r)) { iterate(l, f); f(v); iterate(r, f) }
      }
    };

    // build tree from elements arr[l]..arr[r-1]
    public func buildFromSorted<V>(buf : List.List<V>) : Tree<V> {
      var maxDepth = 0;
      var maxSize = 1;
      while (maxSize < List.size(buf)) {
        maxDepth += 1;
        maxSize += maxSize + 1
      };
      maxDepth := if (maxDepth == 0) { 1 } else { maxDepth }; // keep root black for 1 element tree
      func buildFromSortedHelper(l : Nat, r : Nat, depth : Nat) : Tree<V> {
        if (l + 1 == r) {
          if (depth == maxDepth) {
            return #red(#leaf, List.at(buf, l), #leaf)
          } else {
            return #black(#leaf, List.at(buf, l), #leaf)
          }
        };
        if (l >= r) {
          return #leaf
        };
        let m = (l + r) / 2;
        return #black(
          buildFromSortedHelper(l, m, depth + 1),
          List.at(buf, m),
          buildFromSortedHelper(m + 1, r, depth + 1)
        )
      };
      buildFromSortedHelper(0, List.size(buf), 0)
    };

    type IterRep<T> = Types.Pure.List<{ #tr : Tree<T>; #x : T }>;

    type SetTraverser<T> = (Tree<T>, T, Tree<T>, IterRep<T>) -> IterRep<T>;

    class IterSet<T>(tree : Tree<T>, setTraverser : SetTraverser<T>) {
      var trees : IterRep<T> = ?(#tr(tree), null);
      public func next() : ?T {
        switch (trees) {
          case (null) { null };
          case (?(#tr(#leaf), ts)) {
            trees := ts;
            next()
          };
          case (?(#x(x), ts)) {
            trees := ts;
            ?x
          };
          case (?(#tr(#black(l, x, r)), ts)) {
            trees := setTraverser(l, x, r, ts);
            next()
          };
          case (?(#tr(#red(l, x, r)), ts)) {
            trees := setTraverser(l, x, r, ts);
            next()
          }
        }
      }
    };

    public func iter<T>(s : Tree<T>, direction : { #fwd; #bwd }) : Iter.Iter<T> {
      let turnLeftFirst : SetTraverser<T> = func(l, x, r, ts) {
        ?(#tr(l), ?(#x(x), ?(#tr(r), ts)))
      };

      let turnRightFirst : SetTraverser<T> = func(l, x, r, ts) {
        ?(#tr(r), ?(#x(x), ?(#tr(l), ts)))
      };

      switch direction {
        case (#fwd) IterSet(s, turnLeftFirst);
        case (#bwd) IterSet(s, turnRightFirst)
      }
    };

    public func foldLeft<T, Accum>(
      tree : Tree<T>,
      base : Accum,
      combine : (Accum, T) -> Accum
    ) : Accum {
      switch (tree) {
        case (#leaf) { base };
        case (#black(l, x, r)) {
          let left = foldLeft(l, base, combine);
          let middle = combine(left, x);
          foldLeft(r, middle, combine)
        };
        case (#red(l, x, r)) {
          let left = foldLeft(l, base, combine);
          let middle = combine(left, x);
          foldLeft(r, middle, combine)
        }
      }
    };

    public func foldRight<T, Accum>(
      tree : Tree<T>,
      base : Accum,
      combine : (T, Accum) -> Accum
    ) : Accum {
      switch (tree) {
        case (#leaf) { base };
        case (#black(l, x, r)) {
          let right = foldRight(r, base, combine);
          let middle = combine(x, right);
          foldRight(l, middle, combine)
        };
        case (#red(l, x, r)) {
          let right = foldRight(r, base, combine);
          let middle = combine(x, right);
          foldRight(l, middle, combine)
        }
      }
    };

    func redden<T>(t : Tree<T>) : Tree<T> {
      switch t {
        case (#black(l, x, r)) { (#red(l, x, r)) };
        case _ {
          Runtime.trap "pure/Set.redden() impossible"
        }
      }
    };

    func lbalance<T>(left : Tree<T>, x : T, right : Tree<T>) : Tree<T> {
      switch (left, right) {
        case (#red(#red(l1, x1, r1), x2, r2), r) {
          #red(
            #black(l1, x1, r1),
            x2,
            #black(r2, x, r)
          )
        };
        case (#red(l1, x1, #red(l2, x2, r2)), r) {
          #red(
            #black(l1, x1, l2),
            x2,
            #black(r2, x, r)
          )
        };
        case _ {
          #black(left, x, right)
        }
      }
    };

    func rbalance<T>(left : Tree<T>, x : T, right : Tree<T>) : Tree<T> {
      switch (left, right) {
        case (l, #red(l1, x1, #red(l2, x2, r2))) {
          #red(
            #black(l, x, l1),
            x1,
            #black(l2, x2, r2)
          )
        };
        case (l, #red(#red(l1, x1, r1), x2, r2)) {
          #red(
            #black(l, x, l1),
            x1,
            #black(r1, x2, r2)
          )
        };
        case _ {
          #black(left, x, right)
        }
      }
    };

    public func add<T>(
      set : Set<T>,
      compare : (T, T) -> Order.Order,
      elem : T
    ) : Set<T> {
      insert(set, compare, elem).0
    };

    public func insert<T>(
      s : Set<T>,
      compare : (T, T) -> Order.Order,
      elem : T
    ) : (Set<T>, Bool) {
      var newNodeIsCreated : Bool = false;
      func ins(tree : Tree<T>) : Tree<T> {
        switch tree {
          case (#black(left, x, right)) {
            switch (compare(elem, x)) {
              case (#less) {
                lbalance(ins left, x, right)
              };
              case (#greater) {
                rbalance(left, x, ins right)
              };
              case (#equal) {
                #black(left, x, right)
              }
            }
          };
          case (#red(left, x, right)) {
            switch (compare(elem, x)) {
              case (#less) {
                #red(ins left, x, right)
              };
              case (#greater) {
                #red(left, x, ins right)
              };
              case (#equal) {
                #red(left, x, right)
              }
            }
          };
          case (#leaf) {
            newNodeIsCreated := true;
            #red(#leaf, elem, #leaf)
          }
        }
      };
      let newRoot = switch (ins(s.root)) {
        case (#red(left, x, right)) {
          #black(left, x, right)
        };
        case other { other }
      };
      if newNodeIsCreated ({ root = newRoot; size = s.size + 1 }, true) else (s, false)
    };

    func balLeft<T>(left : Tree<T>, x : T, right : Tree<T>) : Tree<T> {
      switch (left, right) {
        case (#red(l1, x1, r1), r) {
          #red(#black(l1, x1, r1), x, r)
        };
        case (_, #black(l2, x2, r2)) {
          rbalance(left, x, #red(l2, x2, r2))
        };
        case (_, #red(#black(l2, x2, r2), x3, r3)) {
          #red(
            #black(left, x, l2),
            x2,
            rbalance(r2, x3, redden r3)
          )
        };
        case _ { Runtime.trap "pure/Set.balLeft() impossible" }
      }
    };

    func balRight<T>(left : Tree<T>, x : T, right : Tree<T>) : Tree<T> {
      switch (left, right) {
        case (l, #red(l1, x1, r1)) {
          #red(l, x, #black(l1, x1, r1))
        };
        case (#black(l1, x1, r1), r) {
          lbalance(#red(l1, x1, r1), x, r)
        };
        case (#red(l1, x1, #black(l2, x2, r2)), r3) {
          #red(
            lbalance(redden l1, x1, l2),
            x2,
            #black(r2, x, r3)
          )
        };
        case _ { Runtime.trap "pure/Set.balRight() impossible" }
      }
    };

    func append<T>(left : Tree<T>, right : Tree<T>) : Tree<T> {
      switch (left, right) {
        case (#leaf, _) { right };
        case (_, #leaf) { left };
        case (
          #red(l1, x1, r1),
          #red(l2, x2, r2)
        ) {
          switch (append(r1, l2)) {
            case (#red(l3, x3, r3)) {
              #red(
                #red(l1, x1, l3),
                x3,
                #red(r3, x2, r2)
              )
            };
            case r1l2 {
              #red(l1, x1, #red(r1l2, x2, r2))
            }
          }
        };
        case (t1, #red(l2, x2, r2)) {
          #red(append(t1, l2), x2, r2)
        };
        case (#red(l1, x1, r1), t2) {
          #red(l1, x1, append(r1, t2))
        };
        case (#black(l1, x1, r1), #black(l2, x2, r2)) {
          switch (append(r1, l2)) {
            case (#red(l3, x3, r3)) {
              #red(
                #black(l1, x1, l3),
                x3,
                #black(r3, x2, r2)
              )
            };
            case r1l2 {
              balLeft(
                l1,
                x1,
                #black(r1l2, x2, r2)
              )
            }
          }
        }
      }
    };

    public func remove<T>(set : Set<T>, compare : (T, T) -> Order.Order, elem : T) : Set<T> {
      delete(set, compare, elem).0
    };

    public func delete<T>(s : Set<T>, compare : (T, T) -> Order.Order, x : T) : (Set<T>, Bool) {
      var changed : Bool = false;
      func delNode(left : Tree<T>, x1 : T, right : Tree<T>) : Tree<T> {
        switch (compare(x, x1)) {
          case (#less) {
            let newLeft = del left;
            switch left {
              case (#black(_, _, _)) {
                balLeft(newLeft, x1, right)
              };
              case _ {
                #red(newLeft, x1, right)
              }
            }
          };
          case (#greater) {
            let newRight = del right;
            switch right {
              case (#black(_, _, _)) {
                balRight(left, x1, newRight)
              };
              case _ {
                #red(left, x1, newRight)
              }
            }
          };
          case (#equal) {
            changed := true;
            append(left, right)
          }
        }
      };
      func del(tree : Tree<T>) : Tree<T> {
        switch tree {
          case (#black(left, x1, right)) {
            delNode(left, x1, right)
          };
          case (#red(left, x1, right)) {
            delNode(left, x1, right)
          };
          case (#leaf) {
            tree
          }
        }
      };
      let newRoot = switch (del(s.root)) {
        case (#red(left, x1, right)) {
          #black(left, x1, right)
        };
        case other { other }
      };
      if changed ({ root = newRoot; size = s.size - 1 }, true) else (s, false)
    };

    // check binary search tree order of elements and black depth invariant of the RB-tree
    public func assertValid<T>(s : Set<T>, comp : (T, T) -> Order.Order) {
      ignore blackDepth(s.root, comp)
    };

    func blackDepth<T>(node : Tree<T>, comp : (T, T) -> Order.Order) : Nat {
      func checkNode(left : Tree<T>, x1 : T, right : Tree<T>) : Nat {
        checkElem(left, func(x : T) : Bool { comp(x, x1) == #less });
        checkElem(right, func(x : T) : Bool { comp(x, x1) == #greater });
        let leftBlacks = blackDepth(left, comp);
        let rightBlacks = blackDepth(right, comp);
        assert (leftBlacks == rightBlacks);
        leftBlacks
      };
      switch node {
        case (#leaf) 0;
        case (#red(left, x1, right)) {
          assert (not isRed(left));
          assert (not isRed(right));
          checkNode(left, x1, right)
        };
        case (#black(left, x1, right)) {
          checkNode(left, x1, right) + 1
        }
      }
    };

    func isRed<T>(node : Tree<T>) : Bool {
      switch node {
        case (#red(_, _, _)) true;
        case _ false
      }
    };

    func checkElem<T>(node : Tree<T>, isValid : T -> Bool) {
      switch node {
        case (#leaf) {};
        case (#black(_, elem, _)) {
          assert (isValid(elem))
        };
        case (#red(_, elem, _)) {
          assert (isValid(elem))
        }
      }
    }
  };

}
