# core/pure/Map
Immutable, ordered key-value maps.

The map type is stable whenever the key and value types are stable, allowing
map values to be stored in stable variables.

Keys are ordered by an explicit `compare` function, which *must* be the same
across all operations on a given map.


Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  // creation
  let empty = Map.empty<Nat, Text>();
  // insertion
  let map1 = Map.add(empty, Nat.compare, 0, "Zero");
  // retrieval
  assert Map.get(empty, Nat.compare, 0) == null;
  assert Map.get(map1, Nat.compare, 0) == ?"Zero";
  // removal
  let map2 = Map.remove(map1, Nat.compare, 0);
  assert not Map.isEmpty(map1);
  assert Map.isEmpty(map2);
}
```

The internal representation is a red-black tree.

A red-black tree is a balanced binary search tree ordered by the keys.

The tree data structure internally colors each of its nodes either red or black,
and uses this information to balance the tree during the modifying operations.

Performance:
* Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
* Space: `O(n)` for storing the entire tree.
`n` denotes the number of key-value entries (i.e. nodes) stored in the tree.

Note:
* Map operations, such as retrieval, insertion, and removal create `O(log(n))` temporary objects that become garbage.

Credits:

The core of this implementation is derived from:

* Ken Friis Larsen's [RedBlackMap.sml](https://github.com/kfl/mosml/blob/master/src/mosmllib/Redblackmap.sml), which itself is based on:
* Stefan Kahrs, "Red-black trees with types", Journal of Functional Programming, 11(4): 425-432 (2001), [version 1 in web appendix](http://www.cs.ukc.ac.uk/people/staff/smk/redblack/rb.html).

## Type `Map`
``` motoko no-repl
type Map<K, V> = Types.Pure.Map<K, V>
```

@deprecated M0235

## Function `empty`
``` motoko no-repl
func empty<K, V>() : Map<K, V>
```

Create a new empty immutable key-value map.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.empty<Nat, Text>();
  assert Map.size(map) == 0;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `isEmpty`
``` motoko no-repl
func isEmpty<K, V>(self : Map<K, V>) : Bool
```

Determines whether a key-value map is empty.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map0 = Map.empty<Nat, Text>();
  let map1 = Map.add(map0, Nat.compare, 0, "Zero");

  assert Map.isEmpty(map0);
  assert not Map.isEmpty(map1);
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `size`
``` motoko no-repl
func size<K, V>(self : Map<K, V>) : Nat
```

Determine the size of the map as the number of key-value entries.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

assert Map.size(map) == 3;
```

Runtime: `O(n)`.
Space: `O(1)`.

## Function `containsKey`
``` motoko no-repl
func containsKey<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K) : Bool
```

Test whether the map `map`, ordered by `compare`, contains a binding for the given `key`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Map.containsKey(map, Nat.compare, 1);
  assert not Map.containsKey(map, Nat.compare, 42);
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `get`
``` motoko no-repl
func get<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K) : ?V
```

Given, `map` ordered by `compare`, return the value associated with key `key` if present and `null` otherwise.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Map.get(map, Nat.compare, 1) == ?"One";
  assert Map.get(map, Nat.compare, 42) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `insert`
``` motoko no-repl
func insert<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K, value : V) : (Map<K, V>, Bool)
```

Given `map` ordered by `compare`, insert a mapping from `key` to `value`.
Returns the modified map and `true` if the key is new to map, otherwise `false`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map0 = Map.empty<Nat, Text>();

  do {
    let (map1, new1) = Map.insert(map0, Nat.compare, 0, "Zero");
    assert Iter.toArray(Map.entries(map1)) == [(0, "Zero")];
    assert new1;
    let (map2, new2) = Map.insert(map1, Nat.compare, 0, "Nil");
    assert Iter.toArray(Map.entries(map2)) == [(0, "Nil")];
    assert not new2
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `m := Map.add(m, cmp, k, v)`)
causes collecting `O(log(n))` nodes.

## Function `add`
``` motoko no-repl
func add<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K, value : V) : Map<K, V>
```

Given `map` ordered by `compare`, add a new mapping from `key` to `value`.
Replaces any existing entry with key `key`.
Returns the modified map.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  var map = Map.empty<Nat, Text>();

  map := Map.add(map, Nat.compare, 0, "Zero");
  map := Map.add(map, Nat.compare, 1, "One");
  map := Map.add(map, Nat.compare, 0, "Nil");

  assert Iter.toArray(Map.entries(map)) == [(0, "Nil"), (1, "One")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `m := Map.add(m, cmp, k, v)`)
causes collecting `O(log(n))` nodes.

## Function `swap`
``` motoko no-repl
func swap<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K, value : V) : (Map<K, V>, ?V)
```

Given `map` ordered by `compare`, add a mapping from `key` to `value`. Overwrites any existing entry with key `key`.
Returns the modified map and the previous value associated with key `key`
or `null` if no such value exists.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map0 = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  do {
     let (map1, old1) = Map.swap(map0, Nat.compare, 0, "Nil");
     assert Iter.toArray(Map.entries(map1)) == [(0, "Nil"), (1, "One"), (2, "Two")];
     assert old1 == ?"Zero";

     let (map2, old2) = Map.swap(map0, Nat.compare, 3, "Three");
     assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two"), (3, "Three")];
     assert old2 == null;
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `m := Map.swap(m, Nat.compare, k, v).0`)
causes collecting `O(log(n))` nodes.

## Function `replace`
``` motoko no-repl
func replace<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K, value : V) : (Map<K, V>, ?V)
```

Overwrites the value of an existing key and returns the updated map and previous value.
If the key does not exist, returns the original map and `null`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let singleton = Map.singleton(0, "Zero");

  do {
    let (map1, prev1) = Map.replace(singleton, Nat.compare, 0, "Nil"); // overwrites the value for existing key.
    assert prev1 == ?"Zero";
    assert Map.get(map1, Nat.compare, 0) == ?"Nil";

    let (map2, prev2) = Map.replace(map1, Nat.compare, 1, "One");  // no effect, key is absent
    assert prev2 == null;
    assert Map.get(map2, Nat.compare, 1) == null;
 }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `remove`
``` motoko no-repl
func remove<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K) : Map<K, V>
```

Given a `map`, ordered by `compare`, deletes any entry for `key` from `map`.
Has no effect if `key` is not present in the map.
Returns the updated map.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map0 =
    Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  let map1 = Map.remove(map0, Nat.compare, 1);
  assert Iter.toArray(Map.entries(map1)) == [(0, "Zero"), (2, "Two")];
  let map2 = Map.remove(map0, Nat.compare, 42);
  assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `map := Map.delete(map, compare, k).0`)
causes collecting `O(log(n))` nodes.

## Function `delete`
``` motoko no-repl
func delete<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K) : (Map<K, V>, Bool)
```

Given a `map`, ordered by `compare`, deletes any entry for `key` from `map`.
Has no effect if `key` is not present in the map.
Returns the updated map and `true` if the `key` was present in `map`, otherwise `false`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map0 =
    Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  do {
    let (map1, pres1) = Map.delete(map0, Nat.compare, 1);
    assert Iter.toArray(Map.entries(map1)) == [(0, "Zero"), (2, "Two")];
    assert pres1;
    let (map2, pres2) = Map.delete(map0, Nat.compare, 42);
    assert not pres2;
    assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `map := Map.delete(map, compare, k).0`)
causes collecting `O(log(n))` nodes.

## Function `take`
``` motoko no-repl
func take<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), key : K) : (Map<K, V>, ?V)
```

Given a `map`, ordered by `compare`, deletes the entry for `key`. Returns a modified map, leaving `map` unchanged, and the
previous value associated with `key` or `null` if no such value exists.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map0 =  Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  do {
    let (map1, prev1) = Map.take(map0, Nat.compare, 0);
    assert Iter.toArray(Map.entries(map1)) == [(1, "One"), (2, "Two")];
    assert prev1 == ?"Zero";

    let (map2, prev2) = Map.take(map0, Nat.compare, 42);
    assert Iter.toArray(Map.entries(map2)) == [(0, "Zero"), (1, "One"), (2, "Two")];
    assert prev2 == null;
  }
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes.
Garbage collecting one of maps (e.g. after an assignment `map := Map.remove(map, compare, key)`)
causes collecting `O(log(n))` nodes.

## Function `maxEntry`
``` motoko no-repl
func maxEntry<K, V>(self : Map<K, V>) : ?(K, V)
```

Given a `map` retrieves the key-value pair in `map` with a maximal key. If `map` is empty returns `null`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Map.maxEntry(map) == ?(2, "Two");
  assert Map.maxEntry(Map.empty<Nat, Text>()) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `minEntry`
``` motoko no-repl
func minEntry<K, V>(self : Map<K, V>) : ?(K, V)
```

Retrieves a key-value pair from `map` with the minimal key. If the map is empty returns `null`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Map.minEntry(map) == ?(0, "Zero");
  assert Map.minEntry(Map.empty()) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `entries`
``` motoko no-repl
func entries<K, V>(self : Map<K, V>) : Iter.Iter<(K, V)>
```

Returns an Iterator (`Iter`) over the key-value pairs in the map.
Iterator provides a single method `next()`, which returns
pairs in ascending order by keys, or `null` when out of pairs to iterate over.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
  var sum = 0;
  var text = "";
  for ((k, v) in Map.entries(map)) { sum += k; text #= v };
  assert sum == 3;
  assert text == "ZeroOneTwo"
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `reverseEntries`
``` motoko no-repl
func reverseEntries<K, V>(self : Map<K, V>) : Iter.Iter<(K, V)>
```

Returns an Iterator (`Iter`) over the key-value pairs in the map.
Iterator provides a single method `next()`, which returns
pairs in descending order by keys, or `null` when out of pairs to iterate over.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.reverseEntries(map)) == [(2, "Two"), (1, "One"), (0, "Zero")];
  var sum = 0;
  var text = "";
  for ((k, v) in Map.reverseEntries(map)) { sum += k; text #= v };
  assert sum == 3;
  assert text == "TwoOneZero"
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `keys`
``` motoko no-repl
func keys<K, V>(self : Map<K, V>) : Iter.Iter<K>
```

Given a `map`, returns an Iterator (`Iter`) over the keys of the `map`.
Iterator provides a single method `next()`, which returns
keys in ascending order, or `null` when out of keys to iterate over.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.keys(map)) == [0, 1, 2];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `values`
``` motoko no-repl
func values<K, V>(self : Map<K, V>) : Iter.Iter<V>
```

Given a `map`, returns an Iterator (`Iter`) over the values of the map.
Iterator provides a single method `next()`, which returns
values in ascending order of associated keys, or `null` when out of values to iterate over.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.values(map)) == ["Zero", "One", "Two"];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `fromIter`
``` motoko no-repl
func fromIter<K, V>(iter : Iter.Iter<(K, V)>, compare : (implicit : (K, K) -> Order.Order)) : Map<K, V>
```

Returns a new map, containing all entries given by the iterator `i`.
If there are multiple entries with the same key the last one is taken.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  transient let iter =
    Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]);

  let map = Map.fromIter(iter, Nat.compare);

  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `toMap`
``` motoko no-repl
func toMap<K, V>(self : Iter.Iter<(K, V)>, compare : (implicit : (K, K) -> Order.Order)) : Map<K, V>
```

Convert an iterator of entries into a map.
If there are multiple entries with the same key the last one is taken.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  transient let iter =
    Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]);

  let map = iter.toMap(Nat.compare);

  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `map`
``` motoko no-repl
func map<K, V1, V2>(self : Map<K, V1>, f : (K, V1) -> V2) : Map<K, V2>
```

Given a `map` and function `f`, creates a new map by applying `f` to each entry in the map `m`. Each entry
`(k, v)` in the old map is transformed into a new entry `(k, v2)`, where
the new value `v2` is created by applying `f` to `(k, v)`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func f(key : Nat, _val : Text) : Nat = key * 2;

  let resMap = Map.map(map, f);

  assert Iter.toArray(Map.entries(resMap)) == [(0, 0), (1, 2), (2, 4)];
}
```

Cost of mapping all the elements:
Runtime: `O(n)`.
Space: `O(n)` retained memory
where `n` denotes the number of key-value entries stored in the map.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<K, V, A>(self : Map<K, V>, base : A, combine : (A, K, V) -> A) : A
```

Collapses the elements in the `map` into a single value by starting with `base`
and progressively combining keys and values into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func folder(accum : (Nat, Text), key : Nat, val : Text) : ((Nat, Text))
    = (key + accum.0, accum.1 # val);

  assert Map.foldLeft(map, (0, ""), folder) == (3, "ZeroOneTwo");
}
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `foldRight`
``` motoko no-repl
func foldRight<K, V, A>(self : Map<K, V>, base : A, combine : (K, V, A) -> A) : A
```

Collapses the elements in the `map` into a single value by starting with `base`
and progressively combining keys and values into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func folder(key : Nat, val : Text, accum : (Nat, Text)) : ((Nat, Text))
    = (key + accum.0, accum.1 # val);

  assert Map.foldRight(map, (0, ""), folder) == (3, "TwoOneZero");
}
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `all`
``` motoko no-repl
func all<K, V>(self : Map<K, V>, pred : (K, V) -> Bool) : Bool
```

Test whether all key-value pairs in `map` satisfy the given predicate `pred`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);

  assert Map.all<Nat, Text>(map, func (k, v) = v == Nat.toText(k));
  assert not Map.all<Nat, Text>(map, func (k, v) = k < 2);
}
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `any`
``` motoko no-repl
func any<K, V>(self : Map<K, V>, pred : (K, V) -> Bool) : Bool
```

Test if any key-value pair in `map` satisfies the given predicate `pred`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);

  assert Map.any<Nat, Text>(map, func (k, v) = (k >= 0));
  assert not Map.any<Nat, Text>(map, func (k, v) = (k >= 3));
}
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `singleton`
``` motoko no-repl
func singleton<K, V>(key : K, value : V) : Map<K, V>
```

Create a new immutable key-value `map` with a single entry.

Example:
```motoko
import Map "mo:core/pure/Map";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.singleton<Nat, Text>(0, "Zero");
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero")];
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `forEach`
``` motoko no-repl
func forEach<K, V>(self : Map<K, V>, operation : (K, V) -> ())
```

Apply an operation for each key-value pair contained in the map.
The operation is applied in ascending order of the keys.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  var sum = 0;
  var text = "";
  Map.forEach<Nat, Text>(map, func (key, value) {
    sum += key;
    text #= value;
  });
  assert sum == 3;
  assert text == "ZeroOneTwo";
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

## Function `filter`
``` motoko no-repl
func filter<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), criterion : (K, V) -> Bool) : Map<K, V>
```

Filter entries in a new map.
Returns a new map that only contains the key-value pairs
that fulfil the criterion function.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let numberNames = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  let evenNames = Map.filter<Nat, Text>(numberNames, Nat.compare, func (key, value) {
    key % 2 == 0
  });

  assert Iter.toArray(Map.entries(evenNames)) == [(0, "Zero"), (2, "Two")];
}
```

Runtime: `O(n)`.
Space: `O(n)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `filterMap`
``` motoko no-repl
func filterMap<K, V1, V2>(self : Map<K, V1>, compare : (implicit : (K, K) -> Order.Order), f : (K, V1) -> ?V2) : Map<K, V2>
```

Given a `map`, comparison `compare` and function `f`,
constructs a new map ordered by `compare`, by applying `f` to each entry in `map`.
For each entry `(k, v)` in the old map, if `f` evaluates to `null`, the entry is discarded.
Otherwise, the entry is transformed into a new entry `(k, v2)`, where
the new value `v2` is the result of applying `f` to `(k, v)`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func f(key : Nat, val : Text) : ?Text {
    if(key == 0) {null}
    else { ?("Twenty " # val)}
  };

  let newMap = Map.filterMap(map, Nat.compare, f);

  assert Iter.toArray(Map.entries(newMap)) == [(1, "Twenty One"), (2, "Twenty Two")];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `assertValid`
``` motoko no-repl
func assertValid<K, V>(self : Map<K, V>, compare : (implicit : (K, K) -> Order.Order)) : ()
```

Validate the representation invariants of the given `map`.
Assert if any invariants are violated.

## Function `toText`
``` motoko no-repl
func toText<K, V>(self : Map<K, V>, keyFormat : (implicit : (toText : K -> Text)), valueFormat : (implicit : (toText : V -> Text))) : Text
```

Converts the `map` to its textual representation using `keyFormat` and `valueFormat` to convert each key and value to `Text`.

```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  assert Map.toText<Nat, Text>(map, Nat.toText, func t { t }) == "PureMap{(0, Zero), (1, One), (2, Two)}";
}
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `keyFormat` and `valueFormat` run in O(1) time and space.

## Function `equal`
``` motoko no-repl
func equal<K, V>(self : Map<K, V>, other : Map<K, V>, compare : (implicit : (K, K) -> Order.Order), equal : (implicit : (V, V) -> Bool)) : Bool
```

Test whether two immutable maps have equal entries.
Assumes both maps are ordered equivalently.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

persistent actor {
  let map1 = Map.fromIter([(0, "Zero"), (1, "One"), (2, "Two")].values(), Nat.compare);
  let map2 = Map.fromIter<Nat, Text>([(2, "Two"), (1, "One"), (0, "Zero")].values(), Nat.compare);
  assert(Map.equal(map1, map2, Nat.compare, Text.equal));
}
```

Runtime: `O(n)`.
Space: `O(1)`.

## Function `compare`
``` motoko no-repl
func compare<K, V>(self : Map<K, V>, other : Map<K, V>, compareKey : (implicit : (compare : (K, K) -> Order.Order)), compareValue : (implicit : (compare : (V, V) -> Order.Order))) : Order.Order
```

Compare two maps by primarily comparing keys and secondarily values.
Both maps are iterated by the ascending order of their creation and
order is determined by the following rules:
Less:
`map1` is less than `map2` if:
 * the pairwise iteration hits a entry pair `entry1` and `entry2` where
   `entry1` is less than `entry2` and all preceding entry pairs are equal, or,
 * `map1` is  a strict prefix of `map2`, i.e. `map2` has more entries than `map1`
    and all entries of `map1` occur at the beginning of iteration `map2`.
`entry1` is less than `entry2` if:
 * the key of `entry1` is less than the key of `entry2`, or
 * `entry1` and `entry2` have equal keys and the value of `entry1` is less than
   the value of `entry2`.
Equal:
`map1` and `map2` have same series of equal entries by pairwise iteration.
Greater:
`map1` is neither less nor equal `map2`.

Example:
```motoko
import Map "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

persistent actor {
  let map1 = Map.fromIter([(0, "Zero"), (1, "One")].values(), Nat.compare);
  let map2 = Map.fromIter([(0, "Zero"), (2, "Two")].values(), Nat.compare);

  assert Map.compare(map1, map2, Nat.compare, Text.compare) == #less;
  assert Map.compare(map1, map1, Nat.compare, Text.compare) == #equal;
  assert Map.compare(map2, map1, Nat.compare, Text.compare) == #greater
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that `compareKey` and `compareValue` have runtime and space costs of `O(1)`.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.
