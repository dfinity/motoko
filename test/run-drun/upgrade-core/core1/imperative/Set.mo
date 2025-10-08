/// Imperative (mutable) sets based on order/comparison of elements.
/// A set is a collection of elements without duplicates.
/// The set data structure type is stable and can be used for orthogonal persistence.
///
/// Example:
/// ```motoko
/// import Set "mo:core/Set";
/// import Nat "mo:core/Nat";
///
/// persistent actor {
///   let set = Set.fromIter([3, 1, 2, 3].vals(), Nat.compare);
///   assert Set.size(set) == 3;
///   assert not Set.contains(set, Nat.compare, 4);
///   let diff = Set.difference(set, set, Nat.compare);
///   assert Set.isEmpty(diff);
/// }
/// ```
///
/// These sets are implemented as B-trees with order 32, a balanced search tree of ordered elements.
///
/// Performance:
/// * Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
/// * Space: `O(n)` for storing the entire tree,
/// where `n` denotes the number of elements stored in the set.

// Data structure implementation is courtesy of Byron Becker.
// Source: https://github.com/canscale/StableHeapBTreeMap
// Copyright (c) 2022 Byron Becker.
// Distributed under Apache 2.0 license.
// With adjustments by the Motoko team.

import PureSet "../pure/Set";
import Types "../Types";
import Order "../Order";
import Array "../Array";
import VarArray "../VarArray";
import Runtime "../Runtime";
import Stack "../imperative/Stack";
import Option "../Option";
import Iter "../imperative/Iter";
import BTreeHelper "../internal/BTreeHelper";

module {
  let btreeOrder = 32; // Should be >= 4 and <= 512.

  public type Set<T> = Types.Set.Set<T>;
  type Node<T> = Types.Set.Node<T>;
  type Data<T> = Types.Set.Data<T>;
  type Internal<T> = Types.Set.Internal<T>;
  type Leaf<T> = Types.Set.Leaf<T>;

  /// Convert the mutable set to an immutable, purely functional set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import PureSet "mo:core/pure/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([0, 2, 1].values(), Nat.compare);
  ///   let pureSet = Set.toPure(set, Nat.compare);
  ///   assert Iter.toArray(PureSet.values(pureSet)) == Iter.toArray(Set.values(set));
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func toPure<T>(set : Set<T>, compare : (T, T) -> Order.Order) : PureSet.Set<T> {
    PureSet.fromIter(values(set), compare)
  };

  /// Convert an immutable, purely functional set to a mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import PureSet "mo:core/pure/Set";
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let pureSet = PureSet.fromIter([3, 1, 2].values(), Nat.compare);
  ///   let set = Set.fromPure(pureSet, Nat.compare);
  ///   assert Iter.toArray(Set.values(set)) == Iter.toArray(PureSet.values(pureSet));
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func fromPure<T>(set : PureSet.Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    fromIter(PureSet.values(set), compare)
  };

  /// Create a copy of the mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let originalSet = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   let clonedSet = Set.clone(originalSet);
  ///   Set.add(originalSet, Nat.compare, 4);
  ///   assert Set.size(clonedSet) == 3;
  ///   assert Set.size(originalSet) == 4;
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements stored in the set.
  public func clone<T>(set : Set<T>) : Set<T> {
    {
      var root = cloneNode(set.root);
      var size = set.size
    }
  };

  /// Create a new empty mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   assert Set.size(set) == 0;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<T>() : Set<T> {
    {
      var root = #leaf({
        data = {
          elements = VarArray.repeat<?T>(null, btreeOrder - 1);
          var count = 0
        }
      });
      var size = 0
    }
  };

  /// Create a new mutable set with a single element.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  ///
  /// persistent actor {
  ///   let cities = Set.singleton<Text>("Zurich");
  ///   assert Set.size(cities) == 1;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func singleton<T>(element : T) : Set<T> {
    let elements = VarArray.repeat<?T>(null, btreeOrder - 1);
    elements[0] := ?element;
    {
      var root =
      #leaf({ data = { elements; var count = 1 } });
      var size = 1
    }
  };

  /// Remove all the elements from the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Text "mo:core/Text";
  ///
  /// persistent actor {
  ///   let cities = Set.empty<Text>();
  ///   Set.add(cities, Text.compare, "Zurich");
  ///   Set.add(cities, Text.compare, "San Francisco");
  ///   Set.add(cities, Text.compare, "London");
  ///   assert Set.size(cities) == 3;
  ///
  ///   Set.clear(cities);
  ///   assert Set.size(cities) == 0;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func clear<T>(set : Set<T>) {
    let emptySet = empty<T>();
    set.root := emptySet.root;
    set.size := 0
  };

  /// Determines whether a set is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   Set.add(set, Nat.compare, 3);
  ///
  ///   assert not Set.isEmpty(set);
  ///   Set.clear(set);
  ///   assert Set.isEmpty(set);
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func isEmpty<T>(set : Set<T>) : Bool {
    set.size == 0
  };

  /// Return the number of elements in a set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   Set.add(set, Nat.compare, 3);
  ///
  ///   assert Set.size(set) == 3;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func size<T>(set : Set<T>) : Nat {
    set.size
  };

  /// Test whether two imperative sets are equal.
  /// Both sets have to be constructed by the same comparison function.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func equal<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Types.Order) : Bool {
    if (set1.size != set2.size) return false;
    // TODO: optimize
    let iterator1 = values(set1);
    let iterator2 = values(set2);
    loop {
      let next1 = iterator1.next();
      let next2 = iterator2.next();
      switch (next1, next2) {
        case (null, null) {
          return true
        };
        case (?element1, ?element2) {
          if (not (compare(element1, element2) == #equal)) {
            return false
          }
        };
        case _ { return false }
      }
    }
  };

  /// Tests whether the set contains the provided element.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   Set.add(set, Nat.compare, 3);
  ///
  ///   assert Set.contains(set, Nat.compare, 1);
  ///   assert not Set.contains(set, Nat.compare, 4);
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func contains<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Bool {
    switch (set.root) {
      case (#internal(internalNode)) {
        containsInInternal(internalNode, compare, element)
      };
      case (#leaf(leafNode)) { containsInLeaf(leafNode, compare, element) }
    }
  };

  /// Add a new element to a set.
  /// No effect if the element already exists in the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   Set.add(set, Nat.compare, 2);
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   assert Iter.toArray(Set.values(set)) == [1, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func add<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) {
    ignore insert(set, compare, element)
  };

  /// Insert a new element in the set.
  /// Returns true if the element is new, false if the element was already contained in the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   assert Set.insert(set, Nat.compare, 2);
  ///   assert Set.insert(set, Nat.compare, 1);
  ///   assert not Set.insert(set, Nat.compare, 2);
  ///   assert Iter.toArray(Set.values(set)) == [1, 2];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func insert<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Bool {
    let insertResult = switch (set.root) {
      case (#leaf(leafNode)) {
        leafInsertHelper<T>(leafNode, btreeOrder, compare, element)
      };
      case (#internal(internalNode)) {
        internalInsertHelper<T>(internalNode, btreeOrder, compare, element)
      }
    };

    switch (insertResult) {
      case (#inserted) {
        // if inserted an element that was not previously there, increment the tree size counter
        set.size += 1;
        true
      };
      case (#existent) {
        // keep size
        false
      };
      case (#promote({ element = promotedElement; leftChild; rightChild })) {
        let elements = VarArray.repeat<?T>(null, btreeOrder - 1);
        elements[0] := ?promotedElement;
        let children = VarArray.repeat<?Node<T>>(null, btreeOrder);
        children[0] := ?leftChild;
        children[1] := ?rightChild;
        set.root := #internal({
          data = { elements; var count = 1 };
          children
        });
        // promotion always comes from inserting a new element, so increment the tree size counter
        set.size += 1;
        true
      }
    }
  };

  /// Deletes an element from a set.
  /// No effect if the element is not contained in the set.
  ///
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///
  ///   Set.remove(set, Nat.compare, 2);
  ///   assert not Set.contains(set, Nat.compare, 2);
  ///
  ///   Set.remove(set, Nat.compare, 4);
  ///   assert not Set.contains(set, Nat.compare, 4);
  ///
  ///   assert Iter.toArray(Set.values(set)) == [1, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` including garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(log(n))` objects that will be collected as garbage.
  public func remove<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : () {
    ignore delete(set, compare, element)
  };

  /// Deletes an element from a set.
  /// Returns true if the element was contained in the set, false if not.
  ///
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///
  ///   assert Set.delete(set, Nat.compare, 2);
  ///   assert not Set.contains(set, Nat.compare, 2);
  ///
  ///   assert not Set.delete(set, Nat.compare, 4);
  ///   assert not Set.contains(set, Nat.compare, 4);
  ///   assert Iter.toArray(Set.values(set)) == [1, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` including garbage, see below.
  /// where `n` denotes the number of elements stored in the set and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(log(n))` objects that will be collected as garbage.
  public func delete<T>(set : Set<T>, compare : (T, T) -> Order.Order, element : T) : Bool {
    let deleted = switch (set.root) {
      case (#leaf(leafNode)) {
        // TODO: think about how this can be optimized so don't have to do two steps (search and then insert)?
        switch (NodeUtil.getElementIndex<T>(leafNode.data, compare, element)) {
          case (#elementFound(deleteIndex)) {
            leafNode.data.count -= 1;
            ignore BTreeHelper.deleteAndShift<T>(leafNode.data.elements, deleteIndex);
            set.size -= 1;
            true
          };
          case _ { false }
        }
      };
      case (#internal(internalNode)) {
        let deletedElement = switch (internalDeleteHelper(internalNode, btreeOrder, compare, element, false)) {
          case (#deleted) { true };
          case (#inexistent) { false };
          case (#mergeChild({ internalChild })) {
            if (internalChild.data.count > 0) {
              set.root := #internal(internalChild)
            }
            // This case will be hit if the BTree has order == 4
            // In this case, the internalChild has no element (last element was merged with new child), so need to promote that merged child (its only child)
            else {
              set.root := switch (internalChild.children[0]) {
                case (?node) { node };
                case null {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.delete(), element deletion failed, due to a null replacement node error")
                }
              }
            };
            true
          }
        };
        if (deletedElement) {
          // if deleted an element from the BTree, decrement the size
          set.size -= 1
        };
        deletedElement
      }
    };
    deleted
  };

  /// Retrieves the maximum element from the set.
  /// If the set is empty, returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   assert Set.max(set) == null;
  ///   Set.add(set, Nat.compare, 3);
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   assert Set.max(set) == ?3;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the set.
  public func max<T>(set : Set<T>) : ?T {
    reverseValues(set).next()
  };

  /// Retrieves the minimum element from the set.
  /// If the set is empty, returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.empty<Nat>();
  ///   assert Set.min(set) == null;
  ///   Set.add(set, Nat.compare, 1);
  ///   Set.add(set, Nat.compare, 2);
  ///   Set.add(set, Nat.compare, 3);
  ///   assert Set.min(set) == ?1;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of elements stored in the set.
  public func min<T>(set : Set<T>) : ?T {
    values(set).next()
  };

  /// Returns an iterator over the elements in the set,
  /// traversing the elements in the ascending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 2, 3, 1].values(), Nat.compare);
  ///
  ///   var tmp = "";
  ///   for (number in Set.values(set)) {
  ///      tmp #= " " # Nat.toText(number);
  ///   };
  ///   assert tmp == " 0 1 2 3";
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func values<T>(set : Set<T>) : Types.Iter<T> {
    switch (set.root) {
      case (#leaf(leafNode)) { return leafElements(leafNode) };
      case (#internal(internalNode)) { internalElements(internalNode) }
    }
  };

  /// Returns an iterator over the elements in the set,
  /// starting from a given element in ascending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 3, 1].values(), Nat.compare);
  ///   assert Iter.toArray(Set.valuesFrom(set, Nat.compare, 1)) == [1, 3];
  ///   assert Iter.toArray(Set.valuesFrom(set, Nat.compare, 2)) == [3];
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func valuesFrom<T>(
    set : Set<T>,
    compare : (T, T) -> Order.Order,
    element : T
  ) : Types.Iter<T> {
    switch (set.root) {
      case (#leaf(leafNode)) leafElementsFrom(leafNode, compare, element);
      case (#internal(internalNode)) internalElementsFrom(internalNode, compare, element)
    }
  };

  /// Returns an iterator over the elements in the set,
  /// traversing the elements in the descending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  public func reverseValues<T>(set : Set<T>) : Types.Iter<T> {
    switch (set.root) {
      case (#leaf(leafNode)) { return reverseLeafElements(leafNode) };
      case (#internal(internalNode)) { reverseInternalElements(internalNode) }
    }
  };

  /// Returns an iterator over the elements in the set,
  /// starting from a given element in descending order.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 1, 3].values(), Nat.compare);
  ///   assert Iter.toArray(Set.reverseValuesFrom(set, Nat.compare, 0)) == [0];
  ///   assert Iter.toArray(Set.reverseValuesFrom(set, Nat.compare, 2)) == [1, 0];
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func reverseValuesFrom<T>(
    set : Set<T>,
    compare : (T, T) -> Order.Order,
    element : T
  ) : Types.Iter<T> {
    switch (set.root) {
      case (#leaf(leafNode)) reverseLeafElementsFrom(leafNode, compare, element);
      case (#internal(internalNode)) reverseInternalElementsFrom(internalNode, compare, element)
    }
  };

  /// Create a mutable set with the elements obtained from an iterator.
  /// Potential duplicate elements in the iterator are ignored, i.e.
  /// multiple occurrence of an equal element only occur once in the set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([3, 1, 2, 1].values(), Nat.compare);
  ///   assert Iter.toArray(Set.values(set)) == [1, 2, 3];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of elements returned by the iterator and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func fromIter<T>(iter : Types.Iter<T>, compare : (T, T) -> Order.Order) : Set<T> {
    let set = empty<T>();
    for (element in iter) {
      add(set, compare, element)
    };
    set
  };

  /// Test whether `set1` is a sub-set of `set2`, i.e. each element in `set1` is
  /// also contained in `set2`. Returns `true` if both sets are equal.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func isSubset<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Bool {
    if (set1.size > set2.size) { return false };
    // TODO: optimize
    for (element in values(set1)) {
      if (not contains(set2, compare, element)) {
        return false
      }
    };
    true
  };

  /// Returns a new set that is the union of `set1` and `set2`,
  /// i.e. a new set that all the elements that exist in at least on of the two sets.
  /// Potential duplicates are ignored, i.e. if the same element occurs in both `set1`
  /// and `set2`, it only occurs once in the returned set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements stored in the sets `set1` and `set2`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func union<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    let result = clone<T>(set1);
    for (element in values(set2)) {
      if (not contains(result, compare, element)) {
        add(result, compare, element)
      }
    };
    result
  };

  /// Returns a new set that is the intersection of `set1` and `set2`,
  /// i.e. a new set that contains all the elements that exist in both sets.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  public func intersection<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    let result = empty<T>();
    for (element in values(set1)) {
      if (contains(set2, compare, element)) {
        add(result, compare, element)
      }
    };
    result
  };

  /// Returns a new set that is the difference between `set1` and `set2` (`set1` minus `set2`),
  /// i.e. a new set that contains all the elements of `set1` that do not exist in `set2`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  public func difference<T>(set1 : Set<T>, set2 : Set<T>, compare : (T, T) -> Order.Order) : Set<T> {
    let result = empty<T>();
    for (element in values(set1)) {
      if (not contains(set2, compare, element)) {
        add(result, compare, element)
      }
    };
    result
  };

  /// Adds all elements from `iter` to the specified `set`.
  /// This is equivalent to `Set.union()` but modifies the set in place.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([1, 2, 3].values(), Nat.compare);
  ///   Set.addAll(set, Nat.compare, [3, 4, 5].values());
  ///   assert Iter.toArray(Set.values(set)) == [1, 2, 3, 4, 5];
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements in `set` and `iter`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func addAll<T>(set : Set<T>, compare : (T, T) -> Order.Order, iter : Types.Iter<T>) {
    for (element in iter) {
      add(set, compare, element)
    }
  };

  /// Deletes all values in `iter` from the specified `set`.
  /// Returns `true` if any value was present in the set, otherwise false.
  /// The return value indicates whether the size of the set has changed.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 1, 2].values(), Nat.compare);
  ///   assert Set.deleteAll(set, Nat.compare, [0, 2].values());
  ///   assert Iter.toArray(Set.values(set)) == [1];
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements in `set` and `iter`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func deleteAll<T>(set : Set<T>, compare : (T, T) -> Order.Order, iter : Types.Iter<T>) : Bool {
    var deleted = false;
    for (element in iter) {
      deleted := delete(set, compare, element) or deleted // order matters!
    };
    deleted
  };

  /// Inserts all values in `iter` into `set`.
  /// Returns true if any value was not contained in the original set, otherwise false.
  /// The return value indicates whether the size of the set has changed.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([0, 1, 2].values(), Nat.compare);
  ///   assert Set.insertAll(set, Nat.compare, [0, 2, 3].values());
  ///   assert Iter.toArray(Set.values(set)) == [0, 1, 2, 3];
  ///   assert not Set.insertAll(set, Nat.compare, [0, 1, 2].values()); // no change
  /// }
  /// ```
  ///
  /// Runtime: `O(m * log(n))`.
  /// Space: `O(1)` retained memory plus garbage, see the note below.
  /// where `m` and `n` denote the number of elements in `set` and `iter`, respectively,
  /// and assuming that the `compare` function implements an `O(1)` comparison.
  public func insertAll<T>(set : Set<T>, compare : (T, T) -> Order.Order, iter : Types.Iter<T>) : Bool {
    var inserted = false;
    for (element in iter) {
      inserted := insert(set, compare, element) or inserted // order matters!
    };
    inserted
  };

  /// Removes all values in `set` that do not satisfy the given predicate.
  /// Returns `true` if and only if the size of the set has changed.
  /// Modifies the set in place.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter([3, 1, 2].values(), Nat.compare);
  ///
  ///   let sizeChanged = Set.retainAll<Nat>(set, Nat.compare, func n { n % 2 == 0 });
  ///   assert Iter.toArray(Set.values(set)) == [2];
  ///   assert sizeChanged;
  /// }
  /// ```
  public func retainAll<T>(set : Set<T>, compare : (T, T) -> Order.Order, predicate : T -> Bool) : Bool {
    let array = Array.fromIter<T>(values(set));
    deleteAll(
      set,
      compare,
      Iter.filter<T>(array.vals(), func(element : T) : Bool = not predicate(element))
    )
  };

  /// Apply an operation on each element contained in the set.
  /// The operation is applied in ascending order of the elements.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let numbers = Set.fromIter([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   var tmp = "";
  ///   Set.forEach<Nat>(numbers, func (element) {
  ///     tmp #= " " # Nat.toText(element)
  ///   });
  ///   assert tmp == " 0 1 2 3";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func forEach<T>(set : Set<T>, operation : T -> ()) {
    for (element in values(set)) {
      operation(element)
    }
  };

  /// Filter elements in a new set.
  /// Create a copy of the mutable set that only contains the elements
  /// that fulfil the criterion function.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
    let result = empty<T>();
    for (element in values(set)) {
      if (criterion(element)) {
        add(result, compare, element)
      }
    };
    result
  };

  /// Project all elements of the set in a new set.
  /// Apply a mapping function to each element in the set and
  /// collect the mapped elements in a new mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func map<T1, T2>(set : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> T2) : Set<T2> {
    let result = empty<T2>();
    for (element1 in values(set)) {
      let element2 = project(element1);
      add(result, compare, element2)
    };
    result
  };

  /// Filter all elements in the set by also applying a projection to the elements.
  /// Apply a mapping function `project` to all elements in the set and collect all
  /// elements, for which the function returns a non-null new element. Collect all
  /// non-discarded new elements in a new mutable set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  public func filterMap<T1, T2>(set : Set<T1>, compare : (T2, T2) -> Order.Order, project : T1 -> ?T2) : Set<T2> {
    let result = empty<T2>();
    for (element1 in values(set)) {
      switch (project(element1)) {
        case null {};
        case (?element2) add(result, compare, element2)
      }
    };
    result
  };

  /// Iterate all elements in ascending order,
  /// and accumulate the elements by applying the combine function, starting from a base value.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func foldLeft<T, A>(
    set : Set<T>,
    base : A,
    combine : (A, T) -> A
  ) : A {
    var accumulator = base;
    for (element in values(set)) {
      accumulator := combine(accumulator, element)
    };
    accumulator
  };

  /// Iterate all elements in descending order,
  /// and accumulate the elements by applying the combine function, starting from a base value.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func foldRight<T, A>(
    set : Set<T>,
    base : A,
    combine : (T, A) -> A
  ) : A {
    var accumulator = base;
    for (element in reverseValues(set)) {
      accumulator := combine(element, accumulator)
    };
    accumulator
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
  /// import Set "mo:core/Set";
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
  public func join<T>(setIterator : Types.Iter<Set<T>>, compare : (T, T) -> Order.Order) : Set<T> {
    let result = empty<T>();
    for (set in setIterator) {
      for (element in values(set)) {
        add(result, compare, element)
      }
    };
    result
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
  /// import Set "mo:core/Set";
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
    let result = empty<T>();
    for (subSet in values(setOfSets)) {
      for (element in values(subSet)) {
        add(result, compare, element)
      }
    };
    result
  };

  /// Check whether all elements in the set satisfy a predicate, i.e.
  /// the `predicate` function returns `true` for all elements in the set.
  /// Returns `true` for an empty set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func all<T>(set : Set<T>, predicate : T -> Bool) : Bool {
    // TODO optimize, avoiding iterator
    for (element in values(set)) {
      if (not predicate(element)) {
        return false
      }
    };
    true
  };

  /// Check whether at least one element in the set satisfies a predicate, i.e.
  /// the `predicate` function returns `true` for at least one element in the set.
  /// Returns `false` for an empty set.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
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
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of elements stored in the set.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func any<T>(set : Set<T>, predicate : T -> Bool) : Bool {
    // TODO optimize, avoiding iterator
    for (element in values(set)) {
      if (predicate(element)) {
        return true
      }
    };
    false
  };

  /// Internal sanity check function.
  /// Can be used to check that elements have been inserted with a consistent comparison function.
  /// Traps if the internal set structure is invalid.
  public func assertValid<T>(set : Set<T>, compare : (T, T) -> Order.Order) {
    func checkIteration(iterator : Types.Iter<T>, order : Order.Order) {
      switch (iterator.next()) {
        case null {};
        case (?first) {
          var previous = first;
          loop {
            switch (iterator.next()) {
              case null return;
              case (?next) {
                if (compare(previous, next) != order) {
                  Runtime.trap("Invalid order")
                };
                previous := next
              }
            }
          }
        }
      }
    };
    checkIteration(values(set), #less);
    checkIteration(reverseValues(set), #greater)
  };

  /// Generate a textual representation of all the elements in the set.
  /// Primarily to be used for testing and debugging.
  /// The elements are formatted according to `elementFormat`.
  ///
  /// Example:
  /// ```motoko
  /// import Set "mo:core/Set";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let set = Set.fromIter<Nat>([0, 3, 1, 2].values(), Nat.compare);
  ///
  ///   assert Set.toText(set, Nat.toText) == "Set{0, 1, 2, 3}"
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
    var text = "Set{";
    var sep = "";
    for (element in values(set)) {
      text #= sep # elementFormat(element);
      sep := ", "
    };
    text # "}"
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
  /// import Set "mo:core/Set";
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

  func leafElements<T>({ data } : Leaf<T>) : Types.Iter<T> {
    var i : Nat = 0;
    object {
      public func next() : ?T {
        if (i >= data.count) {
          null
        } else {
          let res = data.elements[i];
          i += 1;
          res
        }
      }
    }
  };

  func leafElementsFrom<T>({ data } : Leaf<T>, compare : (T, T) -> Order.Order, element : T) : Types.Iter<T> {
    var i = switch (BinarySearch.binarySearchNode<T>(data.elements, compare, element, data.count)) {
      case (#elementFound(i)) i;
      case (#notFound(i)) i
    };
    object {
      public func next() : ?T {
        if (i >= data.count) {
          null
        } else {
          let res = data.elements[i];
          i += 1;
          res
        }
      }
    }
  };

  func reverseLeafElements<T>({ data } : Leaf<T>) : Types.Iter<T> {
    var i : Nat = data.count;
    object {
      public func next() : ?T {
        if (i == 0) {
          null
        } else {
          let res = data.elements[i - 1];
          i -= 1;
          res
        }
      }
    }
  };

  func reverseLeafElementsFrom<T>({ data } : Leaf<T>, compare : (T, T) -> Order.Order, element : T) : Types.Iter<T> {
    var i = switch (BinarySearch.binarySearchNode<T>(data.elements, compare, element, data.count)) {
      case (#elementFound(i)) i + 1; // +1 to include this element
      case (#notFound(i)) i // i is the index of the first element greater than the search element, or count if all elements are less than the search element
    };
    object {
      public func next() : ?T {
        if (i == 0) {
          null
        } else {
          let res = data.elements[i - 1];
          i -= 1;
          res
        }
      }
    }
  };

  // Cursor type that keeps track of the current node and the current element index in the node
  type NodeCursor<T> = { node : Node<T>; elementIndex : Nat };

  func internalElements<T>(internal : Internal<T>) : Types.Iter<T> {
    // The nodeCursorStack keeps track of the current node and the current element index in the node
    // We use a stack here to push to/pop off the next node cursor to visit
    let nodeCursorStack = initializeForwardNodeCursorStack(internal);
    internalElementsFromStack(nodeCursorStack)
  };

  func internalElementsFrom<T>(internal : Internal<T>, compare : (T, T) -> Order.Order, element : T) : Types.Iter<T> {
    let nodeCursorStack = initializeForwardNodeCursorStackFrom(internal, compare, element);
    internalElementsFromStack(nodeCursorStack)
  };

  func internalElementsFromStack<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>) : Types.Iter<T> {
    object {
      public func next() : ?T {
        // pop the next node cursor off the stack
        var nodeCursor = Stack.pop(nodeCursorStack);
        switch (nodeCursor) {
          case null { return null };
          case (?{ node; elementIndex }) {
            switch (node) {
              // if a leaf node, iterate through the leaf node's next element
              case (#leaf(leafNode)) {
                let lastIndex = leafNode.data.count - 1 : Nat;
                if (elementIndex > lastIndex) {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.internalElements(), leaf elementIndex out of bounds")
                };

                let currentElement = switch (leafNode.data.elements[elementIndex]) {
                  case (?element) { element };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Set.internalElements(), null element found in leaf node."
                      # "leafNode.data.count=" # debug_show (leafNode.data.count) # ", elementIndex=" # debug_show (elementIndex)
                    )
                  }
                };
                // if not at the last element, push the next element index of the leaf onto the stack and return the current element
                if (elementIndex < lastIndex) {
                  Stack.push(
                    nodeCursorStack,
                    {
                      node = #leaf(leafNode);
                      elementIndex = elementIndex + 1 : Nat
                    }
                  )
                };

                ?currentElement
              };
              // if an internal node
              case (#internal(internalNode)) {
                let lastIndex = internalNode.data.count - 1 : Nat;
                // Developer facing message in case of a bug
                if (elementIndex > lastIndex) {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.internalElements(), internal elementIndex out of bounds")
                };

                let currentElement = switch (internalNode.data.elements[elementIndex]) {
                  case (?element) { element };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Set.internalElements(), null element found in internal node. " #
                      "internal.data.count=" # debug_show (internalNode.data.count) # ", elementIndex=" # debug_show (elementIndex)
                    )
                  }
                };

                let nextCursor = {
                  node = #internal(internalNode);
                  elementIndex = elementIndex + 1 : Nat
                };
                // if not the last element, push the next element of the internal node onto the stack
                if (elementIndex < lastIndex) {
                  Stack.push(nodeCursorStack, nextCursor)
                };
                // traverse the next child's min subtree and push the resulting node cursors onto the stack
                // then return the current element of the internal node
                traverseMinSubtreeIter(nodeCursorStack, nextCursor);
                ?currentElement
              }
            }
          }
        }
      }
    }
  };

  func reverseInternalElements<T>(internal : Internal<T>) : Types.Iter<T> {
    // The nodeCursorStack keeps track of the current node and the current element index in the node
    // We use a stack here to push to/pop off the next node cursor to visit
    let nodeCursorStack = initializeReverseNodeCursorStack(internal);
    reverseInternalElementsFromStack(nodeCursorStack)
  };

  func reverseInternalElementsFrom<T>(internal : Internal<T>, compare : (T, T) -> Order.Order, element : T) : Types.Iter<T> {
    let nodeCursorStack = initializeReverseNodeCursorStackFrom(internal, compare, element);
    reverseInternalElementsFromStack(nodeCursorStack)
  };

  func reverseInternalElementsFromStack<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>) : Types.Iter<T> {
    object {
      public func next() : ?T {
        // pop the next node cursor off the stack
        var nodeCursor = Stack.pop(nodeCursorStack);
        switch (nodeCursor) {
          case null { return null };
          case (?{ node; elementIndex }) {
            let firstIndex = 0 : Nat;
            assert (elementIndex > firstIndex);
            switch (node) {
              // if a leaf node, reverse iterate through the leaf node's next element
              case (#leaf(leafNode)) {
                let currentElement = switch (leafNode.data.elements[elementIndex - 1]) {
                  case (?element) { element };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Set.reverseInternalElements(), null element found in leaf node."
                      # "leafNode.data.count=" # debug_show (leafNode.data.count) # ", elementIndex=" # debug_show (elementIndex)
                    )
                  }
                };
                // if not at the last element, push the previous element index of the leaf onto the stack and return the current element
                if (elementIndex - 1 : Nat > firstIndex) {
                  Stack.push(
                    nodeCursorStack,
                    {
                      node = #leaf(leafNode);
                      elementIndex = elementIndex - 1 : Nat
                    }
                  )
                };

                // return the current element
                ?currentElement
              };
              // if an internal node
              case (#internal(internalNode)) {
                let currentElement = switch (internalNode.data.elements[elementIndex - 1]) {
                  case (?element) { element };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Set.reverseInternalElements(), null element found in internal node. " #
                      "internal.data.count=" # debug_show (internalNode.data.count) # ", elementIndex=" # debug_show (elementIndex)
                    )
                  }
                };

                let previousCursor = {
                  node = #internal(internalNode);
                  elementIndex = elementIndex - 1 : Nat
                };
                // if not the first element, push the previous element index of the internal node onto the stack
                if (elementIndex - 1 : Nat > firstIndex) {
                  Stack.push(nodeCursorStack, previousCursor)
                };
                // traverse the previous child's max subtree and push the resulting node cursors onto the stack
                // then return the current element of the internal node
                traverseMaxSubtreeIter(nodeCursorStack, previousCursor);
                ?currentElement
              }
            }
          }
        }
      }
    }
  };

  func initializeForwardNodeCursorStack<T>(internal : Internal<T>) : Stack.Stack<NodeCursor<T>> {
    let nodeCursorStack = Stack.empty<NodeCursor<T>>();
    let nodeCursor : NodeCursor<T> = {
      node = #internal(internal);
      elementIndex = 0
    };

    // push the initial cursor to the stack
    Stack.push(nodeCursorStack, nodeCursor);
    // then traverse left
    traverseMinSubtreeIter(nodeCursorStack, nodeCursor);
    nodeCursorStack
  };

  func initializeForwardNodeCursorStackFrom<T>(internal : Internal<T>, compare : (T, T) -> Order.Order, element : T) : Stack.Stack<NodeCursor<T>> {
    let nodeCursorStack = Stack.empty<NodeCursor<T>>();
    let nodeCursor : NodeCursor<T> = {
      node = #internal(internal);
      elementIndex = 0
    };

    traverseMinSubtreeIterFrom(nodeCursorStack, nodeCursor, compare, element);
    nodeCursorStack
  };

  func initializeReverseNodeCursorStack<T>(internal : Internal<T>) : Stack.Stack<NodeCursor<T>> {
    let nodeCursorStack = Stack.empty<NodeCursor<T>>();
    let nodeCursor : NodeCursor<T> = {
      node = #internal(internal);
      elementIndex = internal.data.count
    };

    // push the initial cursor to the stack
    Stack.push(nodeCursorStack, nodeCursor);
    // then traverse left
    traverseMaxSubtreeIter(nodeCursorStack, nodeCursor);
    nodeCursorStack
  };

  func initializeReverseNodeCursorStackFrom<T>(internal : Internal<T>, compare : (T, T) -> Order.Order, element : T) : Stack.Stack<NodeCursor<T>> {
    let nodeCursorStack = Stack.empty<NodeCursor<T>>();
    let nodeCursor : NodeCursor<T> = {
      node = #internal(internal);
      elementIndex = internal.data.count
    };

    traverseMaxSubtreeIterFrom(nodeCursorStack, nodeCursor, compare, element);
    nodeCursorStack
  };

  // traverse the min subtree of the current node cursor, passing each new element to the node cursor stack
  func traverseMinSubtreeIter<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>, nodeCursor : NodeCursor<T>) {
    var currentNode = nodeCursor.node;
    var childIndex = nodeCursor.elementIndex;

    label l loop {
      switch (currentNode) {
        // If currentNode is leaf, have hit the minimum element of the subtree and already pushed it's cursor to the stack
        // so can return
        case (#leaf(_)) {
          return
        };
        // If currentNode is internal, add it's left most child to the stack and continue traversing
        case (#internal(internalNode)) {
          switch (internalNode.children[childIndex]) {
            // Push the next min (left most) child node to the stack
            case (?childNode) {
              childIndex := 0;
              currentNode := childNode;
              Stack.push(
                nodeCursorStack,
                {
                  node = currentNode;
                  elementIndex = childIndex
                }
              )
            };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.traverseMinSubtreeIter(), null child node error")
            }
          }
        }
      }
    }
  };

  func traverseMinSubtreeIterFrom<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>, nodeCursor : NodeCursor<T>, compare : (T, T) -> Order.Order, element : T) {
    var currentNode = nodeCursor.node;

    label l loop {
      let (node, childrenOption) = switch (currentNode) {
        case (#leaf(leafNode)) (leafNode, null);
        case (#internal(internalNode)) (internalNode, ?internalNode.children)
      };
      let (i, isFound) = switch (NodeUtil.getElementIndex<T>(node.data, compare, element)) {
        case (#elementFound(i)) (i, true);
        case (#notFound(i)) (i, false)
      };
      if (i < node.data.count) {
        Stack.push(
          nodeCursorStack,
          {
            node = currentNode;
            elementIndex = i // greater elements to traverse
          }
        )
      };
      if isFound return;
      let ?children = childrenOption else return;
      let ?childNode = children[i] else Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.traverseMinSubtreeIterFrom(), null child node error");
      currentNode := childNode
    }
  };

  // traverse the max subtree of the current node cursor, passing each new element to the node cursor stack
  func traverseMaxSubtreeIter<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>, nodeCursor : NodeCursor<T>) {
    var currentNode = nodeCursor.node;
    var childIndex = nodeCursor.elementIndex;

    label l loop {
      switch (currentNode) {
        // If currentNode is leaf, have hit the maximum element of the subtree and already pushed it's cursor to the stack
        // so can return
        case (#leaf(_)) {
          return
        };
        // If currentNode is internal, add it's right most child to the stack and continue traversing
        case (#internal(internalNode)) {
          assert (childIndex <= internalNode.data.count); // children are one more than data elements
          switch (internalNode.children[childIndex]) {
            // Push the next max (right most) child node to the stack
            case (?childNode) {
              childIndex := switch (childNode) {
                case (#internal(internalNode)) internalNode.data.count;
                case (#leaf(leafNode)) leafNode.data.count
              };
              currentNode := childNode;
              Stack.push(
                nodeCursorStack,
                {
                  node = currentNode;
                  elementIndex = childIndex
                }
              )
            };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.traverseMaxSubtreeIter(), null child node error")
            }
          }
        }
      }
    }
  };

  func traverseMaxSubtreeIterFrom<T>(nodeCursorStack : Stack.Stack<NodeCursor<T>>, nodeCursor : NodeCursor<T>, compare : (T, T) -> Order.Order, element : T) {
    var currentNode = nodeCursor.node;

    label l loop {
      let (node, childrenOption) = switch (currentNode) {
        case (#leaf(leafNode)) (leafNode, null);
        case (#internal(internalNode)) (internalNode, ?internalNode.children)
      };
      let (i, isFound) = switch (NodeUtil.getElementIndex<T>(node.data, compare, element)) {
        case (#elementFound(i)) (i + 1, true); // +1 to include this element
        case (#notFound(i)) (i, false) // i is the index of the first element less than the search element, or 0 if all elements are greater than the search element
      };
      if (i > 0) {
        Stack.push(
          nodeCursorStack,
          {
            node = currentNode;
            elementIndex = i
          }
        )
      };
      if isFound return;
      let ?children = childrenOption else return;
      let ?childNode = children[i] else Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.traverseMaxSubtreeIterFrom(), null child node error");
      currentNode := childNode
    }
  };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateInternalDeleteResult<T> = {
    // element was deleted
    #deleted;
    // element was absent
    #inexistent;
    // deleted an element, but was unable to successfully borrow and rebalance at the previous level without merging children
    // the internalChild is the merged child that needs to be rebalanced at the next level up in the BTree
    #mergeChild : {
      internalChild : Internal<T>
    }
  };

  func internalDeleteHelper<T>(internalNode : Internal<T>, order : Nat, compare : (T, T) -> Order.Order, deleteElement : T, skipNode : Bool) : IntermediateInternalDeleteResult<T> {
    let minElements = NodeUtil.minElementsFromOrder(order);
    let elementIndex = NodeUtil.getElementIndex<T>(internalNode.data, compare, deleteElement);

    // match on both the result of the node binary search, and if this node level should be skipped even if the element is found (internal element replacement case)
    switch (elementIndex, skipNode) {
      // if element is found in the internal node
      case (#elementFound(deleteIndex), false) {
        if (Option.isNull(internalNode.data.elements[deleteIndex])) {
          Runtime.trap("Bug in Set.internalDeleteHelper")
        };
        // TODO: (optimization) replace with deletion in one step without having to retrieve the max element first
        let replaceElement = NodeUtil.getMaxElement(internalNode.children[deleteIndex]);
        internalNode.data.elements[deleteIndex] := ?replaceElement;
        switch (internalDeleteHelper(internalNode, order, compare, replaceElement, true)) {
          case (#deleted) { #deleted };
          case (#inexistent) { #inexistent };
          case (#mergeChild({ internalChild })) {
            #mergeChild({ internalChild })
          }
        }
      };
      // if element is not found in the internal node OR the element is found, but skipping this node (because deleting the in order precessor i.e. replacement element)
      // in both cases need to descend and traverse to find the element to delete
      case ((#elementFound(_), true) or (#notFound(_), _)) {
        let childIndex = switch (elementIndex) {
          case (#elementFound(replacedSkipElementIndex)) {
            replacedSkipElementIndex
          };
          case (#notFound(childIndex)) { childIndex }
        };
        let child = switch (internalNode.children[childIndex]) {
          case (?c) { c };
          case null {
            Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.internalDeleteHelper, child index of #elementFound or #notfound is null")
          }
        };
        switch (child) {
          // if child is internal
          case (#internal(internalChild)) {
            switch (internalDeleteHelper(internalChild, order, compare, deleteElement, false), childIndex == 0) {
              // if element was successfully deleted and no additional tree re-balancing is needed, return #deleted
              case (#deleted, _) { #deleted };
              case (#inexistent, _) { #inexistent };
              // if internalChild needs rebalancing and pulling child is left most
              case (#mergeChild({ internalChild }), true) {
                // try to pull left-most element and child from right sibling
                switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex + 1, #successor)) {
                  // if can pull up sibling element and child
                  case (#borrowed({ deletedSiblingElement; child })) {
                    NodeUtil.rotateBorrowedElementsAndChildFromSibling(
                      internalNode,
                      childIndex,
                      deletedSiblingElement,
                      child,
                      internalChild,
                      #right
                    );
                    #deleted
                  };
                  // unable to pull from sibling, need to merge with right sibling and push down parent
                  case (#notEnoughElements(sibling)) {
                    // get the parent element that will be pushed down the the child
                    let elementsToBePushedToChild = ?BTreeHelper.deleteAndShift(internalNode.data.elements, 0);
                    internalNode.data.count -= 1;
                    // merge the children and push down the parent
                    let newChild = NodeUtil.mergeChildrenAndPushDownParent<T>(internalChild, elementsToBePushedToChild, sibling);
                    // update children of the parent
                    internalNode.children[0] := ?#internal(newChild);
                    ignore ?BTreeHelper.deleteAndShift(internalNode.children, 1);

                    if (internalNode.data.count < minElements) {
                      #mergeChild({ internalChild = internalNode })
                    } else {
                      #deleted
                    }
                  }
                }
              };
              // if internalChild needs rebalancing and pulling child is > 0, so a left sibling exists
              case (#mergeChild({ internalChild }), false) {
                // try to pull right-most element and its child directly from left sibling
                switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex - 1 : Nat, #predecessor)) {
                  case (#borrowed({ deletedSiblingElement; child })) {
                    NodeUtil.rotateBorrowedElementsAndChildFromSibling(
                      internalNode,
                      childIndex - 1 : Nat,
                      deletedSiblingElement,
                      child,
                      internalChild,
                      #left
                    );
                    #deleted
                  };
                  // unable to pull from left sibling
                  case (#notEnoughElements(leftSibling)) {
                    // if child is not last index, try to pull from the right child
                    if (childIndex < internalNode.data.count) {
                      switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex, #successor)) {
                        // if can pull up sibling element and child
                        case (#borrowed({ deletedSiblingElement; child })) {
                          NodeUtil.rotateBorrowedElementsAndChildFromSibling(
                            internalNode,
                            childIndex,
                            deletedSiblingElement,
                            child,
                            internalChild,
                            #right
                          );
                          return #deleted
                        };
                        // if cannot borrow, from left or right, merge (see below)
                        case _ {}
                      }
                    };

                    // get the parent element that will be pushed down the the child
                    let elementToBePushedToChild = ?BTreeHelper.deleteAndShift(internalNode.data.elements, childIndex - 1 : Nat);
                    internalNode.data.count -= 1;
                    // merge it the children and push down the parent
                    let newChild = NodeUtil.mergeChildrenAndPushDownParent(leftSibling, elementToBePushedToChild, internalChild);

                    // update children of the parent
                    internalNode.children[childIndex - 1] := ?#internal(newChild);
                    ignore ?BTreeHelper.deleteAndShift(internalNode.children, childIndex);

                    if (internalNode.data.count < minElements) {
                      #mergeChild({ internalChild = internalNode })
                    } else {
                      #deleted
                    }
                  }
                }
              }
            }
          };
          // if child is leaf
          case (#leaf(leafChild)) {
            switch (leafDeleteHelper(leafChild, order, compare, deleteElement), childIndex == 0) {
              case (#deleted, _) { #deleted };
              case (#inexistent, _) { #inexistent };
              // if delete child is left most, try to borrow from right child
              case (#mergeLeafData({ leafDeleteIndex }), true) {
                switch (NodeUtil.borrowFromRightLeafChild(internalNode.children, childIndex)) {
                  case (?borrowedElement) {
                    let elementToBePushedToChild = internalNode.data.elements[childIndex];
                    internalNode.data.elements[childIndex] := ?borrowedElement;

                    ignore BTreeHelper.insertAtPostionAndDeleteAtPosition<T>(leafChild.data.elements, elementToBePushedToChild, leafChild.data.count - 1, leafDeleteIndex);
                    #deleted
                  };

                  case null {
                    // can't borrow from right child, delete from leaf and merge with right child and parent element, then push down into new leaf
                    let rightChild = switch (internalNode.children[childIndex + 1]) {
                      case (?#leaf(rc)) { rc };
                      case _ {
                        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.internalDeleteHelper, if trying to borrow from right leaf child is null, rightChild index cannot be null or internal")
                      }
                    };
                    let mergedLeaf = mergeParentWithLeftRightChildLeafNodesAndDelete(
                      internalNode.data.elements[childIndex],
                      leafChild,
                      rightChild,
                      leafDeleteIndex,
                      #left
                    );
                    // delete the left most internal node element, since was merging from a deletion in left most child (0) and the parent element was pushed into the mergedLeaf
                    ignore BTreeHelper.deleteAndShift<T>(internalNode.data.elements, 0);
                    // update internal node children
                    BTreeHelper.replaceTwoWithElementAndShift<Node<T>>(internalNode.children, #leaf(mergedLeaf), 0);
                    internalNode.data.count -= 1;

                    if (internalNode.data.count < minElements) {
                      #mergeChild({
                        internalChild = internalNode
                      })
                    } else {
                      #deleted
                    }

                  }
                }
              };
              // if delete child is middle or right most, try to borrow from left child
              case (#mergeLeafData({ leafDeleteIndex }), false) {
                // if delete child is right most, try to borrow from left child
                switch (NodeUtil.borrowFromLeftLeafChild(internalNode.children, childIndex)) {
                  case (?borrowedElement) {
                    let elementToBePushedToChild = internalNode.data.elements[childIndex - 1];
                    internalNode.data.elements[childIndex - 1] := ?borrowedElement;
                    ignore BTreeHelper.insertAtPostionAndDeleteAtPosition<T>(leafChild.data.elements, elementToBePushedToChild, 0, leafDeleteIndex);
                    #deleted
                  };
                  case null {
                    // if delete child is in the middle, try to borrow from right child
                    if (childIndex < internalNode.data.count) {
                      // try to borrow from right
                      switch (NodeUtil.borrowFromRightLeafChild(internalNode.children, childIndex)) {
                        case (?borrowedElement) {
                          let elementToBePushedToChild = internalNode.data.elements[childIndex];
                          internalNode.data.elements[childIndex] := ?borrowedElement;
                          // insert the successor at the very last element
                          ignore BTreeHelper.insertAtPostionAndDeleteAtPosition<T>(leafChild.data.elements, elementToBePushedToChild, leafChild.data.count - 1, leafDeleteIndex);
                          return #deleted
                        };
                        // if cannot borrow, from left or right, merge (see below)
                        case _ {}
                      }
                    };

                    // can't borrow from left child, delete from leaf and merge with left child and parent element, then push down into new leaf
                    let leftChild = switch (internalNode.children[childIndex - 1]) {
                      case (?#leaf(lc)) { lc };
                      case _ {
                        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.internalDeleteHelper, if trying to borrow from left leaf child is null, then left child index must not be null or internal")
                      }
                    };
                    let mergedLeaf = mergeParentWithLeftRightChildLeafNodesAndDelete(
                      internalNode.data.elements[childIndex - 1],
                      leftChild,
                      leafChild,
                      leafDeleteIndex,
                      #right
                    );
                    // delete the right most internal node element, since was merging from a deletion in the right most child and the parent element was pushed into the mergedLeaf
                    ignore BTreeHelper.deleteAndShift<T>(internalNode.data.elements, childIndex - 1);
                    // update internal node children
                    BTreeHelper.replaceTwoWithElementAndShift<Node<T>>(internalNode.children, #leaf(mergedLeaf), childIndex - 1);
                    internalNode.data.count -= 1;

                    if (internalNode.data.count < minElements) {
                      #mergeChild({
                        internalChild = internalNode
                      })
                    } else {
                      #deleted
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateLeafDeleteResult<T> = {
    // element was deleted
    #deleted;
    // element was absent
    #inexistent;
    // leaf had the minimum number of elements when deleting, so returns the leaf node's data and the index of the element that will be deleted
    #mergeLeafData : {
      data : Data<T>;
      leafDeleteIndex : Nat
    }
  };

  func leafDeleteHelper<T>(leafNode : Leaf<T>, order : Nat, compare : (T, T) -> Order.Order, deleteElement : T) : IntermediateLeafDeleteResult<T> {
    let minElements = NodeUtil.minElementsFromOrder(order);

    switch (NodeUtil.getElementIndex<T>(leafNode.data, compare, deleteElement)) {
      case (#elementFound(deleteIndex)) {
        if (leafNode.data.count > minElements) {
          leafNode.data.count -= 1;
          ignore BTreeHelper.deleteAndShift<T>(leafNode.data.elements, deleteIndex);
          #deleted
        } else {
          #mergeLeafData({
            data = leafNode.data;
            leafDeleteIndex = deleteIndex
          })
        }
      };
      case (#notFound(_)) {
        #inexistent
      }
    }
  };

  func containsInInternal<T>(internalNode : Internal<T>, compare : (T, T) -> Order.Order, element : T) : Bool {
    switch (NodeUtil.getElementIndex<T>(internalNode.data, compare, element)) {
      case (#elementFound _index) {
        true
      };
      case (#notFound(index)) {
        switch (internalNode.children[index]) {
          // expects the child to be there, otherwise there's a bug in binary search or the tree is invalid
          case null { Runtime.trap("Internal bug: Set.containsInInternal") };
          case (?#leaf(leafNode)) { containsInLeaf(leafNode, compare, element) };
          case (?#internal(internalNode)) {
            containsInInternal(internalNode, compare, element)
          }
        }
      }
    }
  };

  func containsInLeaf<T>(leafNode : Leaf<T>, compare : (T, T) -> Order.Order, element : T) : Bool {
    switch (NodeUtil.getElementIndex<T>(leafNode.data, compare, element)) {
      case (#elementFound(_index)) {
        true
      };
      case _ false
    }
  };

  type DeletionSide = { #left; #right };

  func mergeParentWithLeftRightChildLeafNodesAndDelete<T>(
    parentElement : ?T,
    leftChild : Leaf<T>,
    rightChild : Leaf<T>,
    deleteIndex : Nat,
    deletionSide : DeletionSide
  ) : Leaf<T> {
    let count = leftChild.data.count * 2;
    let (elements, _) = BTreeHelper.mergeParentWithChildrenAndDelete<T>(
      parentElement,
      leftChild.data.count,
      leftChild.data.elements,
      rightChild.data.elements,
      deleteIndex,
      deletionSide
    );
    ({
      data = {
        elements;
        var count = count
      }
    })
  };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateInsertResult<T> = {
    // element was inserted
    #inserted;
    // element was alreay present
    #existent;
    // child was full when inserting, so returns the promoted element and the split left and right child
    #promote : {
      element : T;
      leftChild : Node<T>;
      rightChild : Node<T>
    }
  };

  // Helper for inserting into a leaf node
  func leafInsertHelper<T>(leafNode : Leaf<T>, order : Nat, compare : (T, T) -> Order.Order, insertedElement : T) : (IntermediateInsertResult<T>) {
    // Perform binary search to see if the element exists in the node
    switch (NodeUtil.getElementIndex<T>(leafNode.data, compare, insertedElement)) {
      case (#elementFound(insertIndex)) {
        let previous = leafNode.data.elements[insertIndex];
        leafNode.data.elements[insertIndex] := ?insertedElement;
        switch (previous) {
          case (?_) { #existent };
          case null { Runtime.trap("Bug in Set.leafInsertHelper") }; // the binary search already found an element, so this case should never happen
        }
      };
      case (#notFound(insertIndex)) {
        // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
        let maxElements : Nat = order - 1;
        // If the leaf is full, insert, split the node, and promote the middle element
        if (leafNode.data.count >= maxElements) {
          let (leftElements, promotedParentElement, rightElements) = BTreeHelper.insertOneAtIndexAndSplitArray(
            leafNode.data.elements,
            insertedElement,
            insertIndex
          );

          let leftCount = order / 2;
          let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
            leftCount
          };

          (
            #promote({
              element = promotedParentElement;
              leftChild = createLeaf<T>(leftElements, leftCount);
              rightChild = createLeaf<T>(rightElements, rightCount)
            })
          )
        }
        // Otherwise, insert at the specified index (shifting elements over if necessary)
        else {
          NodeUtil.insertAtIndexOfNonFullNodeData<T>(leafNode.data, ?insertedElement, insertIndex);
          #inserted
        }
      }
    }
  };

  // Helper for inserting into an internal node
  func internalInsertHelper<T>(internalNode : Internal<T>, order : Nat, compare : (T, T) -> Order.Order, insertElement : T) : IntermediateInsertResult<T> {
    switch (NodeUtil.getElementIndex<T>(internalNode.data, compare, insertElement)) {
      case (#elementFound(insertIndex)) {
        let previous = internalNode.data.elements[insertIndex];
        internalNode.data.elements[insertIndex] := ?insertElement;
        switch (previous) {
          case (?_) { #existent };
          case null {
            Runtime.trap("Bug in Set.internalInsertHelper, element found")
          }; // the binary search already found an element, so this case should never happen
        }
      };
      case (#notFound(insertIndex)) {
        let insertResult = switch (internalNode.children[insertIndex]) {
          case null {
            Runtime.trap("Bug in Set.internalInsertHelper, not found")
          };
          case (?#leaf(leafNode)) {
            leafInsertHelper(leafNode, order, compare, insertElement)
          };
          case (?#internal(internalChildNode)) {
            internalInsertHelper(internalChildNode, order, compare, insertElement)
          }
        };

        switch (insertResult) {
          case (#inserted) #inserted;
          case (#existent) #existent;
          case (#promote({ element = promotedElement; leftChild; rightChild })) {
            // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
            let maxElements : Nat = order - 1;
            // if current internal node is full, need to split the internal node
            if (internalNode.data.count >= maxElements) {
              // insert and split internal elements, determine new promotion target element
              let (leftElements, promotedParentElement, rightElements) = BTreeHelper.insertOneAtIndexAndSplitArray(
                internalNode.data.elements,
                promotedElement,
                insertIndex
              );

              // calculate the element count in the left elements and the element count in the right elements
              let leftCount = order / 2;
              let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
                leftCount
              };

              // split internal children
              let (leftChildren, rightChildren) = NodeUtil.splitChildrenInTwoWithRebalances<T>(
                internalNode.children,
                insertIndex,
                leftChild,
                rightChild
              );

              // send the element to be promoted, as well as the internal children left and right split
              #promote({
                element = promotedParentElement;
                leftChild = #internal({
                  data = { elements = leftElements; var count = leftCount };
                  children = leftChildren
                });
                rightChild = #internal({
                  data = { elements = rightElements; var count = rightCount };
                  children = rightChildren
                })
              })
            } else {
              // insert the new elements into the internal node
              NodeUtil.insertAtIndexOfNonFullNodeData(internalNode.data, ?promotedElement, insertIndex);
              // split and re-insert the single child that needs rebalancing
              NodeUtil.insertRebalancedChild(internalNode.children, insertIndex, leftChild, rightChild);
              #inserted
            }
          }
        }
      }
    }
  };

  func createLeaf<T>(elements : [var ?T], count : Nat) : Node<T> {
    #leaf({
      data = {
        elements;
        var count
      }
    })
  };

  // FIXME
  // Additional functionality compared to original source.

  func cloneData<T>(data : Data<T>) : Data<T> {
    {
      elements = VarArray.clone(data.elements);
      var count = data.count
    }
  };

  func cloneNode<T>(node : Node<T>) : Node<T> {
    switch node {
      case (#leaf { data }) {
        #leaf { data = cloneData(data) }
      };
      case (#internal { data; children }) {
        let clonedData = cloneData(data);
        let clonedChildren = VarArray.map<?Node<T>, ?Node<T>>(
          children,
          func child {
            switch child {
              case null null;
              case (?childNode) ?cloneNode(childNode)
            }
          }
        );
        #internal({
          data = clonedData;
          children = clonedChildren
        })
      }
    }
  };

  module BinarySearch {
    public type SearchResult = {
      #elementFound : Nat;
      #notFound : Nat
    };

    /// Searches an array for a specific element, returning the index it occurs at if #elementFound, or the child/insert index it may occur at
    /// if #notFound. This is used when determining if a element exists in an internal or leaf node, where an element should be inserted in a
    /// leaf node, or which child of an internal node a element could be in.
    ///
    /// Note: This function expects a mutable, nullable, array of elements in sorted order, where all nulls appear at the end of the array.
    /// This function may trap if a null element appears before any elements. It also expects a maxIndex, which is the right-most index (bound)
    /// from which to begin the binary search (the left most bound is expected to be 0)
    ///
    /// Parameters:
    ///
    /// * array - the sorted array that the binary search is performed upon
    /// * compare - the comparator used to perform the search
    /// * searchElement - the element being compared against in the search
    /// * maxIndex - the right-most index (bound) from which to begin the search
    public func binarySearchNode<T>(array : [var ?T], compare : (T, T) -> Order.Order, searchElement : T, maxIndex : Nat) : SearchResult {
      // TODO: get rid of this check?
      // Trap if array is size 0 (should not happen)
      if (array.size() == 0) {
        assert false
      };

      // if all elements in the array are null (i.e. first element is null), return #notFound(0)
      if (maxIndex == 0) {
        return #notFound(0)
      };

      // Initialize search from first to last index
      var left : Nat = 0;
      var right = maxIndex; // maxIndex does not necessarily mean array.size() - 1
      // Search the array
      while (left < right) {
        let middle = (left + right) / 2;
        switch (array[middle]) {
          case null { assert false };
          case (?element) {
            switch (compare(searchElement, element)) {
              // If the element is present at the middle itself
              case (#equal) { return #elementFound(middle) };
              // If element is greater than mid, it can only be present in left subarray
              case (#greater) { left := middle + 1 };
              // If element is smaller than mid, it can only be present in right subarray
              case (#less) {
                right := if (middle == 0) { 0 } else { middle - 1 }
              }
            }
          }
        }
      };

      if (left == array.size()) {
        return #notFound(left)
      };

      // left == right
      switch (array[left]) {
        // inserting at end of array
        case null { #notFound(left) };
        case (?element) {
          switch (compare(searchElement, element)) {
            // if left is the searched element
            case (#equal) { #elementFound(left) };
            // if the element is not found, return notFound and the insert location
            case (#greater) { #notFound(left + 1) };
            case (#less) { #notFound(left) }
          }
        }
      }
    }
  };

  module NodeUtil {
    /// Inserts element at the given index into a non-full leaf node
    public func insertAtIndexOfNonFullNodeData<T>(data : Data<T>, element : ?T, insertIndex : Nat) {
      let currentLastElementIndex : Nat = if (data.count == 0) { 0 } else {
        data.count - 1
      };
      BTreeHelper.insertAtPosition<T>(data.elements, element, insertIndex, currentLastElementIndex);

      // increment the count of data in this node since just inserted an element
      data.count += 1
    };

    /// Inserts two rebalanced (split) child halves into a non-full array of children.
    public func insertRebalancedChild<T>(children : [var ?Node<T>], rebalancedChildIndex : Nat, leftChildInsert : Node<T>, rightChildInsert : Node<T>) {
      // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
      var j : Nat = children.size() - 2;

      // This is just a sanity check to ensure the children aren't already full (should split promote otherwise)
      // TODO: Remove this check once confident
      if (Option.isSome(children[j + 1])) { assert false };

      // Iterate backwards over the array and shift each element over to the right by one until the rebalancedChildIndex is hit
      while (j > rebalancedChildIndex) {
        children[j + 1] := children[j];
        j -= 1
      };

      // Insert both the left and right rebalanced children (replacing the pre-split child)
      children[j] := ?leftChildInsert;
      children[j + 1] := ?rightChildInsert
    };

    /// Used when splitting the children of an internal node
    ///
    /// Takes in the rebalanced child index, as well as both halves of the rebalanced child and splits the children, inserting the left and right child halves appropriately
    ///
    /// For more context, see the documentation for the splitArrayAndInsertTwo method in ArrayUtils.mo
    public func splitChildrenInTwoWithRebalances<T>(
      children : [var ?Node<T>],
      rebalancedChildIndex : Nat,
      leftChildInsert : Node<T>,
      rightChildInsert : Node<T>
    ) : ([var ?Node<T>], [var ?Node<T>]) {
      BTreeHelper.splitArrayAndInsertTwo<Node<T>>(children, rebalancedChildIndex, leftChildInsert, rightChildInsert)
    };

    /// Helper used to get the element index of of a element within a node
    ///
    /// for more, see the BinarySearch.binarySearchNode() documentation
    public func getElementIndex<T>(data : Data<T>, compare : (T, T) -> Order.Order, element : T) : BinarySearch.SearchResult {
      BinarySearch.binarySearchNode<T>(data.elements, compare, element, data.count)
    };

    // calculates a BTree Node's minimum allowed elements given the order of the BTree
    public func minElementsFromOrder(order : Nat) : Nat {
      if (order % 2 == 0) { order / 2 - 1 } else { order / 2 }
    };

    // Given a node, get the maximum element (right most leaf element)
    public func getMaxElement<T>(node : ?Node<T>) : T {
      switch (node) {
        case (?#leaf({ data })) {
          switch (data.elements[data.count - 1]) {
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.NodeUtil.getMaxElement, data cannot have more elements than it's count")
            };
            case (?element) { element }
          }
        };
        case (?#internal({ data; children })) {
          getMaxElement(children[data.count])
        };
        case null {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.NodeUtil.getMaxElement, the node provided cannot be null")
        }
      }
    };

    type InorderBorrowType = {
      #predecessor;
      #successor
    };

    // attempts to retrieve the in max element of the child leaf node directly to the left if the node will allow it
    // returns the deleted max element if able to retrieve, null if not able
    //
    // mutates the predecessing node's elements
    public func borrowFromLeftLeafChild<T>(children : [var ?Node<T>], ofChildIndex : Nat) : ?T {
      let predecessorIndex : Nat = ofChildIndex - 1;
      borrowFromLeafChild(children, predecessorIndex, #predecessor)
    };

    // attempts to retrieve the in max element of the child leaf node directly to the right if the node will allow it
    // returns the deleted max element if able to retrieve, null if not able
    //
    // mutates the predecessing node's elements
    public func borrowFromRightLeafChild<T>(children : [var ?Node<T>], ofChildIndex : Nat) : ?T {
      borrowFromLeafChild(children, ofChildIndex + 1, #successor)
    };

    func borrowFromLeafChild<T>(children : [var ?Node<T>], borrowChildIndex : Nat, childSide : InorderBorrowType) : ?T {
      let minElements = minElementsFromOrder(children.size());

      switch (children[borrowChildIndex]) {
        case (?#leaf({ data })) {
          if (data.count > minElements) {
            // able to borrow an element from this child, so decrement the count of elements
            data.count -= 1; // Since enforce order >= 4, there will always be at least 1 element per node
            switch (childSide) {
              case (#predecessor) {
                let deletedElement = data.elements[data.count];
                data.elements[data.count] := null;
                deletedElement
              };
              case (#successor) {
                ?BTreeHelper.deleteAndShift(data.elements, 0)
              }
            }
          } else { null }
        };
        case _ {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.NodeUtil.borrowFromLeafChild, the node at the borrow child index cannot be null or internal")
        }
      }
    };

    type InternalBorrowResult<T> = {
      #borrowed : InternalBorrow<T>;
      #notEnoughElements : Internal<T>
    };

    type InternalBorrow<T> = {
      deletedSiblingElement : ?T;
      child : ?Node<T>
    };

    // Attempts to borrow an element and child from an internal sibling node
    public func borrowFromInternalSibling<T>(children : [var ?Node<T>], borrowChildIndex : Nat, borrowType : InorderBorrowType) : InternalBorrowResult<T> {
      let minElements = minElementsFromOrder(children.size());

      switch (children[borrowChildIndex]) {
        case (?#internal({ data; children })) {
          if (data.count > minElements) {
            data.count -= 1;
            switch (borrowType) {
              case (#predecessor) {
                let deletedSiblingElement = data.elements[data.count];
                data.elements[data.count] := null;
                let child = children[data.count + 1];
                children[data.count + 1] := null;
                #borrowed({
                  deletedSiblingElement;
                  child
                })
              };
              case (#successor) {
                #borrowed({
                  deletedSiblingElement = ?BTreeHelper.deleteAndShift(data.elements, 0);
                  child = ?BTreeHelper.deleteAndShift(children, 0)
                })
              }
            }
          } else { #notEnoughElements({ data; children }) }
        };
        case _ {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Set.NodeUtil.borrowFromInternalSibling from internal sibling, the child at the borrow index cannot be null or a leaf")
        }
      }
    };

    type SiblingSide = { #left; #right };

    // Rotates the borrowed elements and child from sibling side of the internal node to the internal child recipient
    public func rotateBorrowedElementsAndChildFromSibling<T>(
      internalNode : Internal<T>,
      parentRotateIndex : Nat,
      borrowedSiblingElement : ?T,
      borrowedSiblingChild : ?Node<T>,
      internalChildRecipient : Internal<T>,
      siblingSide : SiblingSide
    ) {
      // if borrowing from the left, the rotated element and child will always be inserted first
      // if borrowing from the right, the rotated element and child will always be inserted last
      let (elementIndex, childIndex) = switch (siblingSide) {
        case (#left) { (0, 0) };
        case (#right) {
          (internalChildRecipient.data.count, internalChildRecipient.data.count + 1)
        }
      };

      // get the parent element that will be pushed down the the child
      let elementToBePushedToChild = internalNode.data.elements[parentRotateIndex];
      // replace the parent with the sibling element
      internalNode.data.elements[parentRotateIndex] := borrowedSiblingElement;
      // push the element and child down into the internalChild
      insertAtIndexOfNonFullNodeData<T>(internalChildRecipient.data, elementToBePushedToChild, elementIndex);

      BTreeHelper.insertAtPosition<Node<T>>(internalChildRecipient.children, borrowedSiblingChild, childIndex, internalChildRecipient.data.count)
    };

    // Merges the elements and children of two internal nodes, pushing the parent element in between the right and left halves
    public func mergeChildrenAndPushDownParent<T>(leftChild : Internal<T>, parentElement : ?T, rightChild : Internal<T>) : Internal<T> {
      {
        data = mergeData<T>(leftChild.data, parentElement, rightChild.data);
        children = mergeChildren(leftChild.children, rightChild.children)
      }
    };

    func mergeData<T>(leftData : Data<T>, parentElement : ?T, rightData : Data<T>) : Data<T> {
      assert leftData.count <= minElementsFromOrder(leftData.elements.size() + 1);
      assert rightData.count <= minElementsFromOrder(rightData.elements.size() + 1);

      let mergedElements = VarArray.repeat<?T>(null, leftData.elements.size());
      var i = 0;
      while (i < leftData.count) {
        mergedElements[i] := leftData.elements[i];
        i += 1
      };

      mergedElements[i] := parentElement;
      i += 1;

      var j = 0;
      while (j < rightData.count) {
        mergedElements[i] := rightData.elements[j];
        i += 1;
        j += 1
      };

      {
        elements = mergedElements;
        var count = leftData.count + 1 + rightData.count
      }
    };

    func mergeChildren<T>(leftChildren : [var ?Node<T>], rightChildren : [var ?Node<T>]) : [var ?Node<T>] {
      let mergedChildren = VarArray.repeat<?Node<T>>(null, leftChildren.size());
      var i = 0;

      while (Option.isSome(leftChildren[i])) {
        mergedChildren[i] := leftChildren[i];
        i += 1
      };

      var j = 0;
      while (Option.isSome(rightChildren[j])) {
        mergedChildren[i] := rightChildren[j];
        i += 1;
        j += 1
      };

      mergedChildren
    }
  }
}
