/// Immutable, ordered key-value maps.
///
/// The map type is stable whenever the key and value types are stable, allowing
/// map values to be stored in stable variables.
///
/// Keys are ordered by an explicit `compare` function, which *must* be the same
/// across all operations on a given map.
///
///
/// Example:
/// ```motoko
/// import Map "mo:core/pure/Map";
/// import Nat "mo:core/Nat";
///
/// persistent actor {
///   // creation
///   let empty = Map.empty<Nat, Text>();
///   // insertion
///   let map1 = Map.add(empty, Nat.compare, 0, "Zero");
///   // retrieval
///   assert Map.get(empty, Nat.compare, 0) == null;
///   assert Map.get(map1, Nat.compare, 0) == ?"Zero";
///   // removal
///   let map2 = Map.remove(map1, Nat.compare, 0);
///   assert not Map.isEmpty(map1);
///   assert Map.isEmpty(map2);
/// }
/// ```
///
/// The internal representation is a red-black tree.
///
/// A red-black tree is a balanced binary search tree ordered by the keys.
///
/// The tree data structure internally colors each of its nodes either red or black,
/// and uses this information to balance the tree during the modifying operations.
///
/// Performance:
/// * Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
/// * Space: `O(n)` for storing the entire tree.
/// `n` denotes the number of key-value entries (i.e. nodes) stored in the tree.
///
/// Note:
/// * Map operations, such as retrieval, insertion, and removal create `O(log(n))` temporary objects that become garbage.
///
/// Credits:
///
/// The core of this implementation is derived from:
///
/// * Ken Friis Larsen's [RedBlackMap.sml](https://github.com/kfl/mosml/blob/master/src/mosmllib/Redblackmap.sml), which itself is based on:
/// * Stefan Kahrs, "Red-black trees with types", Journal of Functional Programming, 11(4): 425-432 (2001), [version 1 in web appendix](http://www.cs.ukc.ac.uk/people/staff/smk/redblack/rb.html).

import Order "../Order";
import Iter "../imperative/Iter";
import Types "../Types";
import Runtime "../Runtime";

// TODO: inline Internal?
// TODO: Do we want clone or clear, just to match imperative API?
// inline Tree type, remove Types.Pure.Tree?

module {

  public type Map<K, V> = Types.Pure.Map<K, V>;

  type Tree<K, V> = Types.Pure.Map.Tree<K, V>;

  /// Create a new empty immutable key-value map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   assert Map.size(map) == 0;
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<K, V>() : Map<K, V> {
    Internal.empty<K, V>()
  };

  /// Determines whether a key-value map is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map0 = Map.empty<Nat, Text>();
  ///   let map1 = Map.add(map0, Nat.compare, 0, "Zero");
  ///
  ///   assert Map.isEmpty(map0);
  ///   assert not Map.isEmpty(map1);
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func isEmpty<K, V>(map : Map<K, V>) : Bool {
    map.size == 0
  };

  /// Determine the size of the map as the number of key-value entries.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  /// assert Map.size(map) == 3;
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func size<K, V>(map : Map<K, V>) : Nat = map.size;

  /// Test whether the map `map`, ordered by `compare`, contains a binding for the given `key`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Map.containsKey(map, Nat.compare, 1);
  ///   assert not Map.containsKey(map, Nat.compare, 42);
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func containsKey<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Bool = Internal.contains(map.root, compare, key);

  /// Given, `map` ordered by `compare`, return the value associated with key `key` if present and `null` otherwise.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Map.get(map, Nat.compare, 1) == ?"One";
  ///   assert Map.get(map, Nat.compare, 42) == null;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func get<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V = Internal.get(map.root, compare, key);

  /// Given `map` ordered by `compare`, insert a mapping from `key` to `value`.
  /// Returns the modified map and `true` if the key is new to map, otherwise `false`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map0 = Map.empty<Nat, Text>();
  ///
  ///   do {
  ///     let (map1, new1) = Map.insert(map0, Nat.compare, 0, "Zero");
  ///     assert Iter.toArray(Map.entries(map1)) == [(0, "Zero")];
  ///     assert new1;
  ///     let (map2, new2) = Map.insert(map1, Nat.compare, 0, "Nil");
  ///     assert Iter.toArray(Map.entries(map2)) == [(0, "Nil")];
  ///     assert not new2
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `m := Map.add(m, cmp, k, v)`)
  /// causes collecting `O(log(n))` nodes.
  public func insert<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : (Map<K, V>, Bool) {
    switch (swap(map, compare, key, value)) {
      case (map1, null) (map1, true);
      case (map1, _) (map1, false)
    }
  };

  /// Given `map` ordered by `compare`, add a new mapping from `key` to `value`.
  /// Replaces any existing entry with key `key`.
  /// Returns the modified map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   var map = Map.empty<Nat, Text>();
  ///
  ///   map := Map.add(map, Nat.compare, 0, "Zero");
  ///   map := Map.add(map, Nat.compare, 1, "One");
  ///   map := Map.add(map, Nat.compare, 0, "Nil");
  ///
  ///   assert Iter.toArray(Map.entries(map)) == [(0, "Nil"), (1, "One")];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `m := Map.add(m, cmp, k, v)`)
  /// causes collecting `O(log(n))` nodes.
  public func add<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : Map<K, V> {
    swap(map, compare, key, value).0
  };

  /// Given `map` ordered by `compare`, add a mapping from `key` to `value`. Overwrites any existing entry with key `key`.
  /// Returns the modified map and the previous value associated with key `key`
  /// or `null` if no such value exists.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map0 = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   do {
  ///      let (map1, old1) = Map.swap(map0, Nat.compare, 0, "Nil");
  ///      assert Iter.toArray(Map.entries(map1)) == [(0, "Nil"), (1, "One"), (2, "Two")];
  ///      assert old1 == ?"Zero";
  ///
  ///      let (map2, old2) = Map.swap(map0, Nat.compare, 3, "Three");
  ///      assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two"), (3, "Three")];
  ///      assert old2 == null;
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `m := Map.swap(m, Nat.compare, k, v).0`)
  /// causes collecting `O(log(n))` nodes.
  public func swap<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : (Map<K, V>, ?V) {
    switch (Internal.swap(map.root, compare, key, value)) {
      case (t, null) { ({ root = t; size = map.size + 1 }, null) };
      case (t, v) { ({ root = t; size = map.size }, v) }
    }
  };

  /// Overwrites the value of an existing key and returns the updated map and previous value.
  /// If the key does not exist, returns the original map and `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let singleton = Map.singleton(0, "Zero");
  ///
  ///   do {
  ///     let (map1, prev1) = Map.replace(singleton, Nat.compare, 0, "Nil"); // overwrites the value for existing key.
  ///     assert prev1 == ?"Zero";
  ///     assert Map.get(map1, Nat.compare, 0) == ?"Nil";
  ///
  ///     let (map2, prev2) = Map.replace(map1, Nat.compare, 1, "One");  // no effect, key is absent
  ///     assert prev2 == null;
  ///     assert Map.get(map2, Nat.compare, 1) == null;
  ///  }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func replace<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : (Map<K, V>, ?V) {
    // TODO: Could be optimized in future
    if (containsKey(map, compare, key)) {
      swap(map, compare, key, value)
    } else { (map, null) }
  };

  /// Given a `map`, ordered by `compare`, deletes any entry for `key` from `map`.
  /// Has no effect if `key` is not present in the map.
  /// Returns the updated map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map0 =
  ///     Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   let map1 = Map.remove(map0, Nat.compare, 1);
  ///   assert Iter.toArray(Map.entries(map1)) == [(0, "Zero"), (2, "Two")];
  ///   let map2 = Map.remove(map0, Nat.compare, 42);
  ///   assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `map := Map.delete(map, compare, k).0`)
  /// causes collecting `O(log(n))` nodes.
  public func remove<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Map<K, V> {
    switch (Internal.remove(map.root, compare, key)) {
      case (_, null) map;
      case (t, ?_) { { root = t; size = map.size - 1 } }
    }
  };

  /// Given a `map`, ordered by `compare`, deletes any entry for `key` from `map`.
  /// Has no effect if `key` is not present in the map.
  /// Returns the updated map and `true` if the `key` was present in `map`, otherwise `false`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map0 =
  ///     Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   do {
  ///     let (map1, pres1) = Map.delete(map0, Nat.compare, 1);
  ///     assert Iter.toArray(Map.entries(map1)) == [(0, "Zero"), (2, "Two")];
  ///     assert pres1;
  ///     let (map2, pres2) = Map.delete(map0, Nat.compare, 42);
  ///     assert not pres2;
  ///     assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `map := Map.delete(map, compare, k).0`)
  /// causes collecting `O(log(n))` nodes.
  public func delete<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : (Map<K, V>, Bool) {
    switch (Internal.remove(map.root, compare, key)) {
      case (_, null) { (map, false) };
      case (t, ?_) { ({ root = t; size = map.size - 1 }, true) }
    }
  };

  /// Given a `map`, ordered by `compare`, deletes the entry for `key`. Returns a modified map, leaving `map` unchanged, and the
  /// previous value associated with `key` or `null` if no such value exists.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map0 =  Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   do {
  ///     let (map1, prev1) = Map.take(map0, Nat.compare, 0);
  ///     assert Iter.toArray(Map.entries(map1)) == [(1, "One"), (2, "Two")];
  ///     assert prev1 == ?"Zero";
  ///
  ///     let (map2, prev2) = Map.take(map0, Nat.compare, 42);
  ///     assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  ///     assert prev2 == null;
  ///   }
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: The returned map shares with the `m` most of the tree nodes.
  /// Garbage collecting one of maps (e.g. after an assignment `map := Map.remove(map, compare, key)`)
  /// causes collecting `O(log(n))` nodes.
  public func take<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : (Map<K, V>, ?V) {
    switch (Internal.remove(map.root, compare, key)) {
      case (t, null) { ({ root = t; size = map.size }, null) };
      case (t, v) { ({ root = t; size = map.size - 1 }, v) }
    }
  };

  /// Given a `map` retrieves the key-value pair in `map` with a maximal key. If `map` is empty returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Map.maxEntry(map) == ?(2, "Two");
  ///   assert Map.maxEntry(Map.empty<Nat, Text>()) == null;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func maxEntry<K, V>(map : Map<K, V>) : ?(K, V) = Internal.maxEntry(map.root);

  /// Retrieves a key-value pair from `map` with the minimal key. If the map is empty returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Map.minEntry(map) == ?(0, "Zero");
  ///   assert Map.minEntry(Map.empty()) == null;
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func minEntry<K, V>(map : Map<K, V>) : ?(K, V) = Internal.minEntry(map.root);

  /// Returns an Iterator (`Iter`) over the key-value pairs in the map.
  /// Iterator provides a single method `next()`, which returns
  /// pairs in ascending order by keys, or `null` when out of pairs to iterate over.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  ///   var sum = 0;
  ///   var text = "";
  ///   for ((k, v) in Map.entries(map)) { sum += k; text #= v };
  ///   assert sum == 3;
  ///   assert text == "ZeroOneTwo"
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(log(n))` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func entries<K, V>(map : Map<K, V>) : Iter.Iter<(K, V)> = Internal.iter(map.root, #fwd);

  /// Returns an Iterator (`Iter`) over the key-value pairs in the map.
  /// Iterator provides a single method `next()`, which returns
  /// pairs in descending order by keys, or `null` when out of pairs to iterate over.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Iter.toArray(Map.reverseEntries(map)) == [(2, "Two"), (1, "One"), (0, "Zero")];
  ///   var sum = 0;
  ///   var text = "";
  ///   for ((k, v) in Map.reverseEntries(map)) { sum += k; text #= v };
  ///   assert sum == 3;
  ///   assert text == "TwoOneZero"
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(log(n))` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func reverseEntries<K, V>(map : Map<K, V>) : Iter.Iter<(K, V)> = Internal.iter(map.root, #bwd);

  /// Given a `map`, returns an Iterator (`Iter`) over the keys of the `map`.
  /// Iterator provides a single method `next()`, which returns
  /// keys in ascending order, or `null` when out of keys to iterate over.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Iter.toArray(Map.keys(map)) == [0, 1, 2];
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(log(n))` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func keys<K, V>(map : Map<K, V>) : Iter.Iter<K> = Iter.map(entries(map), func(kv : (K, V)) : K { kv.0 });

  /// Given a `map`, returns an Iterator (`Iter`) over the values of the map.
  /// Iterator provides a single method `next()`, which returns
  /// values in ascending order of associated keys, or `null` when out of values to iterate over.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  /// let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   assert Iter.toArray(Map.values(map)) == ["Zero", "One", "Two"];
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(log(n))` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func values<K, V>(map : Map<K, V>) : Iter.Iter<V> = Iter.map(entries(map), func(kv : (K, V)) : V { kv.1 });

  /// Returns a new map, containing all entries given by the iterator `i`.
  /// If there are multiple entries with the same key the last one is taken.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   transient let iter =
  ///     Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]);
  ///
  ///   let map = Map.fromIter(iter, Nat.compare);
  ///
  ///   assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func fromIter<K, V>(iter : Iter.Iter<(K, V)>, compare : (K, K) -> Order.Order) : Map<K, V> = Internal.fromIter(iter, compare);

  /// Given a `map` and function `f`, creates a new map by applying `f` to each entry in the map `m`. Each entry
  /// `(k, v)` in the old map is transformed into a new entry `(k, v2)`, where
  /// the new value `v2` is created by applying `f` to `(k, v)`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   func f(key : Nat, _val : Text) : Nat = key * 2;
  ///
  ///   let resMap = Map.map(map, f);
  ///
  ///   assert Iter.toArray(Map.entries(resMap)) == [(0, 0), (1, 2), (2, 4)];
  /// }
  /// ```
  ///
  /// Cost of mapping all the elements:
  /// Runtime: `O(n)`.
  /// Space: `O(n)` retained memory
  /// where `n` denotes the number of key-value entries stored in the map.
  public func map<K, V1, V2>(map : Map<K, V1>, f : (K, V1) -> V2) : Map<K, V2> = Internal.map(map, f);

  /// Collapses the elements in the `map` into a single value by starting with `base`
  /// and progressively combining keys and values into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   func folder(accum : (Nat, Text), key : Nat, val : Text) : ((Nat, Text))
  ///     = (key + accum.0, accum.1 # val);
  ///
  ///   assert Map.foldLeft(map, (0, ""), folder) == (3, "ZeroOneTwo");
  /// }
  /// ```
  ///
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: depends on `combine` function plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func foldLeft<K, V, A>(
    map : Map<K, V>,
    base : A,
    combine : (A, K, V) -> A
  ) : A = Internal.foldLeft(map.root, base, combine);

  /// Collapses the elements in the `map` into a single value by starting with `base`
  /// and progressively combining keys and values into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   func folder(key : Nat, val : Text, accum : (Nat, Text)) : ((Nat, Text))
  ///     = (key + accum.0, accum.1 # val);
  ///
  ///   assert Map.foldRight(map, (0, ""), folder) == (3, "TwoOneZero");
  /// }
  /// ```
  ///
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: depends on `combine` function plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map.
  ///
  /// Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.
  public func foldRight<K, V, A>(
    map : Map<K, V>,
    base : A,
    combine : (K, V, A) -> A
  ) : A = Internal.foldRight(map.root, base, combine);

  /// Test whether all key-value pairs in `map` satisfy the given predicate `pred`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);
  ///
  ///   assert Map.all<Nat, Text>(map, func (k, v) = v == Nat.toText(k));
  ///   assert not Map.all<Nat, Text>(map, func (k, v) = k < 2);
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func all<K, V>(map : Map<K, V>, pred : (K, V) -> Bool) : Bool = Internal.all(map.root, pred);

  /// Test if any key-value pair in `map` satisfies the given predicate `pred`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);
  ///
  ///   assert Map.any<Nat, Text>(map, func (k, v) = (k >= 0));
  ///   assert not Map.any<Nat, Text>(map, func (k, v) = (k >= 3));
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func any<K, V>(map : Map<K, V>, pred : (K, V) -> Bool) : Bool = Internal.any(map.root, pred);

  /// Create a new immutable key-value `map` with a single entry.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.singleton<Nat, Text>(0, "Zero");
  ///   assert Iter.toArray(Map.entries(map)) == [(0, "Zero")];
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func singleton<K, V>(key : K, value : V) : Map<K, V> {
    {
      size = 1;
      root = #red(#leaf, key, value, #leaf)
    }
  };

  /// Apply an operation for each key-value pair contained in the map.
  /// The operation is applied in ascending order of the keys.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///   var sum = 0;
  ///   var text = "";
  ///   Map.forEach<Nat, Text>(map, func (key, value) {
  ///     sum += key;
  ///     text #= value;
  ///   });
  ///   assert sum == 3;
  ///   assert text == "ZeroOneTwo";
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func forEach<K, V>(map : Map<K, V>, operation : (K, V) -> ()) = Internal.forEach(map, operation);

  /// Filter entries in a new map.
  /// Returns a new map that only contains the key-value pairs
  /// that fulfil the criterion function.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let numberNames = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   let evenNames = Map.filter<Nat, Text>(numberNames, Nat.compare, func (key, value) {
  ///     key % 2 == 0
  ///   });
  ///
  ///   assert Iter.toArray(Map.entries(evenNames)) == [(0, "Zero"), (2, "Two")];
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func filter<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, criterion : (K, V) -> Bool) : Map<K, V> = Internal.filter(map, compare, criterion);

  /// Given a `map`, comparison `compare` and function `f`,
  /// constructs a new map ordered by `compare`, by applying `f` to each entry in `map`.
  /// For each entry `(k, v)` in the old map, if `f` evaluates to `null`, the entry is discarded.
  /// Otherwise, the entry is transformed into a new entry `(k, v2)`, where
  /// the new value `v2` is the result of applying `f` to `(k, v)`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///
  ///   func f(key : Nat, val : Text) : ?Text {
  ///     if(key == 0) {null}
  ///     else { ?("Twenty " # val)}
  ///   };
  ///
  ///   let newMap = Map.filterMap(map, Nat.compare, f);
  ///
  ///   assert Iter.toArray(Map.entries(newMap)) == [(1, "Twenty One"), (2, "Twenty Two")];
  /// }
  /// ```
  ///
  /// Runtime: `O(n * log(n))`.
  /// Space: `O(n)` retained memory plus garbage, see the note below.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  public func filterMap<K, V1, V2>(map : Map<K, V1>, compare : (K, K) -> Order.Order, f : (K, V1) -> ?V2) : Map<K, V2> = Internal.mapFilter(map, compare : (K, K) -> Order.Order, f);

  /// Validate the representation invariants of the given `map`.
  /// Assert if any invariants are violated.
  public func assertValid<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order) : () = Internal.validate(map, compare);

  /// Converts the `map` to its textual representation using `keyFormat` and `valueFormat` to convert each key and value to `Text`.
  ///
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  ///
  /// persistent actor {
  ///   let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  ///   assert Map.toText<Nat, Text>(map, Nat.toText, func t { t }) == "PureMap{(0, Zero), (1, One), (2, Two)}";
  /// }
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `keyFormat` and `valueFormat` run in O(1) time and space.
  public func toText<K, V>(map : Map<K, V>, keyFormat : K -> Text, valueFormat : V -> Text) : Text {
    var text = "PureMap{";
    var sep = "";
    for ((k, v) in entries(map)) {
      text #= sep # "(" # keyFormat(k) # ", " # valueFormat(v) # ")";
      sep := ", "
    };
    text # "}"
  };

  /// Test whether two immutable maps have equal entries.
  /// Assumes both maps are ordered equivalently.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Text "mo:core/Text";
  ///
  /// persistent actor {
  ///   let map1 = Map.fromIter([(0, "Zero"), (1, "One"), (2, "Two")].values(), Nat.compare);
  ///   let map2 = Map.fromIter<Nat, Text>([(2, "Two"), (1, "One"), (0, "Zero")].values(), Nat.compare);
  ///   assert(Map.equal(map1, map2, Nat.compare, Text.equal));
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func equal<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compareKey : (K, K) -> Order.Order, equalValue : (V, V) -> Bool) : Bool {
    if (map1.size != map2.size) {
      return false
    };
    let iterator1 = entries(map1);
    let iterator2 = entries(map2);
    loop {
      let next1 = iterator1.next();
      let next2 = iterator2.next();
      switch (next1, next2) {
        case (null, null) {
          return true
        };
        case (?(key1, value1), ?(key2, value2)) {
          if (not (compareKey(key1, key2) == #equal) or not equalValue(value1, value2)) {
            return false
          }
        };
        case _ { return false }
      }
    }
  };

  /// Compare two maps by primarily comparing keys and secondarily values.
  /// Both maps are iterated by the ascending order of their creation and
  /// order is determined by the following rules:
  /// Less:
  /// `map1` is less than `map2` if:
  ///  * the pairwise iteration hits a entry pair `entry1` and `entry2` where
  ///    `entry1` is less than `entry2` and all preceding entry pairs are equal, or,
  ///  * `map1` is  a strict prefix of `map2`, i.e. `map2` has more entries than `map1`
  ///     and all entries of `map1` occur at the beginning of iteration `map2`.
  /// `entry1` is less than `entry2` if:
  ///  * the key of `entry1` is less than the key of `entry2`, or
  ///  * `entry1` and `entry2` have equal keys and the value of `entry1` is less than
  ///    the value of `entry2`.
  /// Equal:
  /// `map1` and `map2` have same series of equal entries by pairwise iteration.
  /// Greater:
  /// `map1` is neither less nor equal `map2`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:core/pure/Map";
  /// import Nat "mo:core/Nat";
  /// import Text "mo:core/Text";
  ///
  /// persistent actor {
  ///   let map1 = Map.fromIter([(0, "Zero"), (1, "One")].values(), Nat.compare);
  ///   let map2 = Map.fromIter([(0, "Zero"), (2, "Two")].values(), Nat.compare);
  ///
  ///   assert Map.compare(map1, map2, Nat.compare, Text.compare) == #less;
  ///   assert Map.compare(map1, map1, Nat.compare, Text.compare) == #equal;
  ///   assert Map.compare(map2, map1, Nat.compare, Text.compare) == #greater
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)` retained memory plus garbage, see below.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that `compareKey` and `compareValue` have runtime and space costs of `O(1)`.
  ///
  /// Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
  public func compare<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compareKey : (K, K) -> Order.Order, compareValue : (V, V) -> Order.Order) : Order.Order {
    let iterator1 = entries(map1);
    let iterator2 = entries(map2);
    loop {
      switch (iterator1.next(), iterator2.next()) {
        case (null, null) return #equal;
        case (null, _) return #less;
        case (_, null) return #greater;
        case (?(key1, value1), ?(key2, value2)) {
          let keyComparison = compareKey(key1, key2);
          if (keyComparison != #equal) {
            return keyComparison
          };
          let valueComparison = compareValue(value1, value2);
          if (valueComparison != #equal) {
            return valueComparison
          }
        }
      }
    }
  };

  module Internal {

    public func empty<K, V>() : Map<K, V> { { size = 0; root = #leaf } };

    public func fromIter<K, V>(i : Iter.Iter<(K, V)>, compare : (K, K) -> Order.Order) : Map<K, V> {
      var map = #leaf : Tree<K, V>;
      var size = 0;
      for (val in i) {
        map := add(map, compare, val.0, val.1);
        size += 1
      };
      { root = map; size }
    };

    type List<T> = Types.Pure.List<T>;

    type IterRep<K, V> = List<{ #tr : Tree<K, V>; #xy : (K, V) }>;

    public func iter<K, V>(map : Tree<K, V>, direction : { #fwd; #bwd }) : Iter.Iter<(K, V)> {
      let turnLeftFirst : MapTraverser<K, V> = func(l, x, y, r, ts) {
        ?(#tr(l), ?(#xy(x, y), ?(#tr(r), ts)))
      };

      let turnRightFirst : MapTraverser<K, V> = func(l, x, y, r, ts) {
        ?(#tr(r), ?(#xy(x, y), ?(#tr(l), ts)))
      };

      switch direction {
        case (#fwd) IterMap(map, turnLeftFirst);
        case (#bwd) IterMap(map, turnRightFirst)
      }
    };

    type MapTraverser<K, V> = (Tree<K, V>, K, V, Tree<K, V>, IterRep<K, V>) -> IterRep<K, V>;

    class IterMap<K, V>(tree : Tree<K, V>, mapTraverser : MapTraverser<K, V>) {
      var trees : IterRep<K, V> = ?(#tr(tree), null);
      public func next() : ?(K, V) {
        switch (trees) {
          case (null) { null };
          case (?(#tr(#leaf), ts)) {
            trees := ts;
            next()
          };
          case (?(#xy(xy), ts)) {
            trees := ts;
            ?xy
          };
          case (?(#tr(#red(l, x, y, r)), ts)) {
            trees := mapTraverser(l, x, y, r, ts);
            next()
          };
          case (?(#tr(#black(l, x, y, r)), ts)) {
            trees := mapTraverser(l, x, y, r, ts);
            next()
          }
        }
      }
    };

    public func map<K, V1, V2>(map : Map<K, V1>, f : (K, V1) -> V2) : Map<K, V2> {
      func mapRec(m : Tree<K, V1>) : Tree<K, V2> {
        switch m {
          case (#leaf) { #leaf };
          case (#red(l, x, y, r)) {
            #red(mapRec l, x, f(x, y), mapRec r)
          };
          case (#black(l, x, y, r)) {
            #black(mapRec l, x, f(x, y), mapRec r)
          }
        }
      };
      { size = map.size; root = mapRec(map.root) }
    };

    public func foldLeft<Key, Value, Accum>(
      map : Tree<Key, Value>,
      base : Accum,
      combine : (Accum, Key, Value) -> Accum
    ) : Accum {
      switch (map) {
        case (#leaf) { base };
        case (#red(l, k, v, r)) {
          let left = foldLeft(l, base, combine);
          let middle = combine(left, k, v);
          foldLeft(r, middle, combine)
        };
        case (#black(l, k, v, r)) {
          let left = foldLeft(l, base, combine);
          let middle = combine(left, k, v);
          foldLeft(r, middle, combine)
        }
      }
    };

    public func foldRight<Key, Value, Accum>(
      map : Tree<Key, Value>,
      base : Accum,
      combine : (Key, Value, Accum) -> Accum
    ) : Accum {
      switch (map) {
        case (#leaf) { base };
        case (#red(l, k, v, r)) {
          let right = foldRight(r, base, combine);
          let middle = combine(k, v, right);
          foldRight(l, middle, combine)
        };
        case (#black(l, k, v, r)) {
          let right = foldRight(r, base, combine);
          let middle = combine(k, v, right);
          foldRight(l, middle, combine)
        }
      }
    };

    public func forEach<K, V>(map : Map<K, V>, operation : (K, V) -> ()) {
      func combine(_acc : Null, key : K, value : V) : Null {
        operation(key, value);
        null
      };
      ignore foldLeft(map.root, null, combine)
    };

    public func filter<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, criterion : (K, V) -> Bool) : Map<K, V> {
      var size = 0;
      func combine(acc : Tree<K, V>, key : K, value : V) : Tree<K, V> {
        if (criterion(key, value)) {
          size += 1;
          add(acc, compare, key, value)
        } else acc
      };
      { root = foldLeft(map.root, #leaf, combine); size }
    };

    public func mapFilter<K, V1, V2>(map : Map<K, V1>, compare : (K, K) -> Order.Order, f : (K, V1) -> ?V2) : Map<K, V2> {
      var size = 0;
      func combine(acc : Tree<K, V2>, key : K, value1 : V1) : Tree<K, V2> {
        switch (f(key, value1)) {
          case null { acc };
          case (?value2) {
            size += 1;
            add(acc, compare, key, value2)
          }
        }
      };
      { root = foldLeft(map.root, #leaf, combine); size }
    };

    public func get<K, V>(t : Tree<K, V>, compare : (K, K) -> Order.Order, x : K) : ?V {
      switch t {
        case (#red(l, x1, y1, r)) {
          switch (compare(x, x1)) {
            case (#less) { get(l, compare, x) };
            case (#equal) { ?y1 };
            case (#greater) { get(r, compare, x) }
          }
        };
        case (#black(l, x1, y1, r)) {
          switch (compare(x, x1)) {
            case (#less) { get(l, compare, x) };
            case (#equal) { ?y1 };
            case (#greater) { get(r, compare, x) }
          }
        };
        case (#leaf) { null }
      }
    };

    public func contains<K, V>(m : Tree<K, V>, compare : (K, K) -> Order.Order, key : K) : Bool {
      switch (get(m, compare, key)) {
        case (null) { false };
        case (_) { true }
      }
    };

    public func maxEntry<K, V>(m : Tree<K, V>) : ?(K, V) {
      func rightmost(m : Tree<K, V>) : (K, V) {
        switch m {
          case (#red(_, k, v, #leaf)) { (k, v) };
          case (#red(_, _, _, r)) { rightmost(r) };
          case (#black(_, k, v, #leaf)) { (k, v) };
          case (#black(_, _, _, r)) { rightmost(r) };
          case (#leaf) { Runtime.trap "pure/Map.maxEntry() impossible" }
        }
      };
      switch m {
        case (#leaf) { null };
        case (_) { ?rightmost(m) }
      }
    };

    public func minEntry<K, V>(m : Tree<K, V>) : ?(K, V) {
      func leftmost(m : Tree<K, V>) : (K, V) {
        switch m {
          case (#red(#leaf, k, v, _)) { (k, v) };
          case (#red(l, _, _, _)) { leftmost(l) };
          case (#black(#leaf, k, v, _)) { (k, v) };
          case (#black(l, _, _, _)) { leftmost(l) };
          case (#leaf) { Runtime.trap "pure/Map.minEntry() impossible" }
        }
      };
      switch m {
        case (#leaf) { null };
        case (_) { ?leftmost(m) }
      }
    };

    public func all<K, V>(m : Tree<K, V>, pred : (K, V) -> Bool) : Bool {
      switch m {
        case (#red(l, k, v, r)) {
          pred(k, v) and all(l, pred) and all(r, pred)
        };
        case (#black(l, k, v, r)) {
          pred(k, v) and all(l, pred) and all(r, pred)
        };
        case (#leaf) { true }
      }
    };

    public func any<K, V>(m : Tree<K, V>, pred : (K, V) -> Bool) : Bool {
      switch m {
        case (#red(l, k, v, r)) {
          pred(k, v) or any(l, pred) or any(r, pred)
        };
        case (#black(l, k, v, r)) {
          pred(k, v) or any(l, pred) or any(r, pred)
        };
        case (#leaf) { false }
      }
    };

    func redden<K, V>(t : Tree<K, V>) : Tree<K, V> {
      switch t {
        case (#black(l, x, y, r)) { (#red(l, x, y, r)) };
        case _ {
          Runtime.trap "pure/Map.redden() impossible"
        }
      }
    };

    func lbalance<K, V>(left : Tree<K, V>, x : K, y : V, right : Tree<K, V>) : Tree<K, V> {
      switch (left, right) {
        case (#red(#red(l1, x1, y1, r1), x2, y2, r2), r) {
          #red(
            #black(l1, x1, y1, r1),
            x2,
            y2,
            #black(r2, x, y, r)
          )
        };
        case (#red(l1, x1, y1, #red(l2, x2, y2, r2)), r) {
          #red(
            #black(l1, x1, y1, l2),
            x2,
            y2,
            #black(r2, x, y, r)
          )
        };
        case _ {
          #black(left, x, y, right)
        }
      }
    };

    func rbalance<K, V>(left : Tree<K, V>, x : K, y : V, right : Tree<K, V>) : Tree<K, V> {
      switch (left, right) {
        case (l, #red(l1, x1, y1, #red(l2, x2, y2, r2))) {
          #red(
            #black(l, x, y, l1),
            x1,
            y1,
            #black(l2, x2, y2, r2)
          )
        };
        case (l, #red(#red(l1, x1, y1, r1), x2, y2, r2)) {
          #red(
            #black(l, x, y, l1),
            x1,
            y1,
            #black(r1, x2, y2, r2)
          )
        };
        case _ {
          #black(left, x, y, right)
        }
      }
    };

    type ClashResolver<A> = { old : A; new : A } -> A;

    func insertWith<K, V>(
      m : Tree<K, V>,
      compare : (K, K) -> Order.Order,
      key : K,
      val : V,
      onClash : ClashResolver<V>
    ) : Tree<K, V> {
      func ins(tree : Tree<K, V>) : Tree<K, V> {
        switch tree {
          case (#black(left, x, y, right)) {
            switch (compare(key, x)) {
              case (#less) {
                lbalance(ins left, x, y, right)
              };
              case (#greater) {
                rbalance(left, x, y, ins right)
              };
              case (#equal) {
                let newVal = onClash({ new = val; old = y });
                #black(left, key, newVal, right)
              }
            }
          };
          case (#red(left, x, y, right)) {
            switch (compare(key, x)) {
              case (#less) {
                #red(ins left, x, y, right)
              };
              case (#greater) {
                #red(left, x, y, ins right)
              };
              case (#equal) {
                let newVal = onClash { new = val; old = y };
                #red(left, key, newVal, right)
              }
            }
          };
          case (#leaf) {
            #red(#leaf, key, val, #leaf)
          }
        }
      };
      switch (ins m) {
        case (#red(left, x, y, right)) {
          #black(left, x, y, right)
        };
        case other { other }
      }
    };

    public func swap<K, V>(
      m : Tree<K, V>,
      compare : (K, K) -> Order.Order,
      key : K,
      val : V
    ) : (Tree<K, V>, ?V) {
      var oldVal : ?V = null;
      func onClash(clash : { old : V; new : V }) : V {
        oldVal := ?clash.old;
        clash.new
      };
      let res = insertWith(m, compare, key, val, onClash);
      (res, oldVal)
    };

    public func add<K, V>(
      m : Tree<K, V>,
      compare : (K, K) -> Order.Order,
      key : K,
      val : V
    ) : Tree<K, V> = swap(m, compare, key, val).0;

    func balLeft<K, V>(left : Tree<K, V>, x : K, y : V, right : Tree<K, V>) : Tree<K, V> {
      switch (left, right) {
        case (#red(l1, x1, y1, r1), r) {
          #red(
            #black(l1, x1, y1, r1),
            x,
            y,
            r
          )
        };
        case (_, #black(l2, x2, y2, r2)) {
          rbalance(left, x, y, #red(l2, x2, y2, r2))
        };
        case (_, #red(#black(l2, x2, y2, r2), x3, y3, r3)) {
          #red(
            #black(left, x, y, l2),
            x2,
            y2,
            rbalance(r2, x3, y3, redden r3)
          )
        };
        case _ { Runtime.trap "pure/Map.balLeft() impossible" }
      }
    };

    func balRight<K, V>(left : Tree<K, V>, x : K, y : V, right : Tree<K, V>) : Tree<K, V> {
      switch (left, right) {
        case (l, #red(l1, x1, y1, r1)) {
          #red(
            l,
            x,
            y,
            #black(l1, x1, y1, r1)
          )
        };
        case (#black(l1, x1, y1, r1), r) {
          lbalance(#red(l1, x1, y1, r1), x, y, r)
        };
        case (#red(l1, x1, y1, #black(l2, x2, y2, r2)), r3) {
          #red(
            lbalance(redden l1, x1, y1, l2),
            x2,
            y2,
            #black(r2, x, y, r3)
          )
        };
        case _ { Runtime.trap "pure/Map.balRight() impossible" }
      }
    };

    func append<K, V>(left : Tree<K, V>, right : Tree<K, V>) : Tree<K, V> {
      switch (left, right) {
        case (#leaf, _) { right };
        case (_, #leaf) { left };
        case (
          #red(l1, x1, y1, r1),
          #red(l2, x2, y2, r2)
        ) {
          switch (append(r1, l2)) {
            case (#red(l3, x3, y3, r3)) {
              #red(
                #red(l1, x1, y1, l3),
                x3,
                y3,
                #red(r3, x2, y2, r2)
              )
            };
            case r1l2 {
              #red(l1, x1, y1, #red(r1l2, x2, y2, r2))
            }
          }
        };
        case (t1, #red(l2, x2, y2, r2)) {
          #red(append(t1, l2), x2, y2, r2)
        };
        case (#red(l1, x1, y1, r1), t2) {
          #red(l1, x1, y1, append(r1, t2))
        };
        case (#black(l1, x1, y1, r1), #black(l2, x2, y2, r2)) {
          switch (append(r1, l2)) {
            case (#red(l3, x3, y3, r3)) {
              #red(
                #black(l1, x1, y1, l3),
                x3,
                y3,
                #black(r3, x2, y2, r2)
              )
            };
            case r1l2 {
              balLeft(
                l1,
                x1,
                y1,
                #black(r1l2, x2, y2, r2)
              )
            }
          }
        }
      }
    };

    public func delete<K, V>(m : Tree<K, V>, compare : (K, K) -> Order.Order, key : K) : Tree<K, V> = remove(m, compare, key).0;

    public func remove<K, V>(tree : Tree<K, V>, compare : (K, K) -> Order.Order, x : K) : (Tree<K, V>, ?V) {
      var y0 : ?V = null;
      func delNode(left : Tree<K, V>, x1 : K, y1 : V, right : Tree<K, V>) : Tree<K, V> {
        switch (compare(x, x1)) {
          case (#less) {
            let newLeft = del left;
            switch left {
              case (#black(_, _, _, _)) {
                balLeft(newLeft, x1, y1, right)
              };
              case _ {
                #red(newLeft, x1, y1, right)
              }
            }
          };
          case (#greater) {
            let newRight = del right;
            switch right {
              case (#black(_, _, _, _)) {
                balRight(left, x1, y1, newRight)
              };
              case _ {
                #red(left, x1, y1, newRight)
              }
            }
          };
          case (#equal) {
            y0 := ?y1;
            append(left, right)
          }
        }
      };
      func del(tree : Tree<K, V>) : Tree<K, V> {
        switch tree {
          case (#red(left, x, y, right)) {
            delNode(left, x, y, right)
          };
          case (#black(left, x, y, right)) {
            delNode(left, x, y, right)
          };
          case (#leaf) {
            tree
          }
        }
      };
      switch (del(tree)) {
        case (#red(left, x, y, right)) { (#black(left, x, y, right), y0) };
        case other { (other, y0) }
      }
    };

    // Test helper
    public func validate<K, V>(rbMap : Map<K, V>, comp : (K, K) -> Order.Order) {
      ignore blackDepth(rbMap.root, comp)
    };

    func blackDepth<K, V>(node : Tree<K, V>, comp : (K, K) -> Order.Order) : Nat {
      func checkNode(left : Tree<K, V>, key : K, right : Tree<K, V>) : Nat {
        checkKey(left, func(x : K) : Bool { comp(x, key) == #less });
        checkKey(right, func(x : K) : Bool { comp(x, key) == #greater });
        let leftBlacks = blackDepth(left, comp);
        let rightBlacks = blackDepth(right, comp);
        assert (leftBlacks == rightBlacks);
        leftBlacks
      };
      switch node {
        case (#leaf) 0;
        case (#red(left, key, _, right)) {
          let leftBlacks = checkNode(left, key, right);
          assert (not isRed(left));
          assert (not isRed(right));
          leftBlacks
        };
        case (#black(left, key, _, right)) {
          checkNode(left, key, right) + 1
        }
      }
    };

    func isRed<K, V>(node : Tree<K, V>) : Bool {
      switch node {
        case (#red(_, _, _, _)) true;
        case _ false
      }
    };

    func checkKey<K, V>(node : Tree<K, V>, isValid : K -> Bool) {
      switch node {
        case (#leaf) {};
        case (#red(_, key, _, _)) {
          assert (isValid(key))
        };
        case (#black(_, key, _, _)) {
          assert (isValid(key))
        }
      }
    }
  };

}
