# core/Map
An imperative key-value map based on order/comparison of the keys.
The map data structure type is stable and can be used for orthogonal persistence.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  // creation
  let map = Map.empty<Nat, Text>();
  // insertion
  Map.add(map, Nat.compare, 0, "Zero");
  // retrieval
  assert Map.get(map, Nat.compare, 0) == ?"Zero";
  assert Map.get(map, Nat.compare, 1) == null;
  // removal
  Map.remove(map, Nat.compare, 0);
  assert Map.isEmpty(map);
}
```

The internal implementation is a B-tree with order 32.

Performance:
* Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
* Space: `O(n)` for storing the entire map.
`n` denotes the number of key-value entries stored in the map.

## Type `Map`
``` motoko no-repl
type Map<K, V> = Types.Map<K, V>
```


## Function `toPure`
``` motoko no-repl
func toPure<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order) : PureMap.Map<K, V>
```

Convert the mutable key-value map to an immutable key-value map.

Example:
```motoko
import Map "mo:core/Map";
import PureMap "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(), Nat.compare);
  let pureMap = Map.toPure(map, Nat.compare);
  assert Iter.toArray(PureMap.entries(pureMap)) == Iter.toArray(Map.entries(map))
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.

## Function `fromPure`
``` motoko no-repl
func fromPure<K, V>(map : PureMap.Map<K, V>, compare : (K, K) -> Order.Order) : Map<K, V>
```

Convert an immutable key-value map to a mutable key-value map.

Example:
```motoko
import Map "mo:core/Map";
import PureMap "mo:core/pure/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let pureMap = PureMap.fromIter(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(), Nat.compare);
  let map = Map.fromPure<Nat, Text>(pureMap, Nat.compare);
  assert Iter.toArray(Map.entries(map)) == Iter.toArray(PureMap.entries(pureMap))
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `clone`
``` motoko no-repl
func clone<K, V>(map : Map<K, V>) : Map<K, V>
```

Create a copy of the mutable key-value map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let originalMap = Map.fromIter<Nat, Text>(
    [(1, "One"), (2, "Two"), (3, "Three")].values(), Nat.compare);
  let clonedMap = Map.clone(originalMap);
  Map.add(originalMap, Nat.compare, 4, "Four");
  assert Map.size(clonedMap) == 3;
  assert Map.size(originalMap) == 4;
}
```

Runtime: `O(n)`.
Space: `O(n)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `empty`
``` motoko no-repl
func empty<K, V>() : Map<K, V>
```

Create a new empty mutable key-value map.

Example:
```motoko
import Map "mo:core/Map";

persistent actor {
  let map = Map.empty<Nat, Text>();
  assert Map.size(map) == 0;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `singleton`
``` motoko no-repl
func singleton<K, V>(key : K, value : V) : Map<K, V>
```

Create a new mutable key-value map with a single entry.

Example:
```motoko
import Map "mo:core/Map";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.singleton<Nat, Text>(0, "Zero");
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero")];
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `clear`
``` motoko no-repl
func clear<K, V>(map : Map<K, V>)
```

Delete all the entries in the key-value map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);

  assert Map.size(map) == 3;

  Map.clear(map);
  assert Map.size(map) == 0;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `isEmpty`
``` motoko no-repl
func isEmpty<K, V>(map : Map<K, V>) : Bool
```

Determines whether a key-value map is empty.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);

  assert not Map.isEmpty(map);
  Map.clear(map);
  assert Map.isEmpty(map);
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `size`
``` motoko no-repl
func size<K, V>(map : Map<K, V>) : Nat
```

Return the number of entries in a key-value map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);

  assert Map.size(map) == 3;
  Map.clear(map);
  assert Map.size(map) == 0;
}
```

Runtime: `O(1)`.
Space: `O(1)`.

## Function `equal`
``` motoko no-repl
func equal<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compareKey : (K, K) -> Types.Order, equalValue : (V, V) -> Bool) : Bool
```

Test whether two imperative maps have equal entries.
Both maps have to be constructed by the same comparison function.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

persistent actor {
  let map1 = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);
  let map2 = Map.clone(map1);

  assert Map.equal(map1, map2, Nat.compare, Text.equal);
  Map.clear(map2);
  assert not Map.equal(map1, map2, Nat.compare, Text.equal);
}
```

Runtime: `O(n)`.
Space: `O(1)`.

## Function `containsKey`
``` motoko no-repl
func containsKey<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Bool
```

Tests whether the map contains the provided key.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);

  assert Map.containsKey(map, Nat.compare, 1);
  assert not Map.containsKey(map, Nat.compare, 3);
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `get`
``` motoko no-repl
func get<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V
```

Get the value associated with key in the given map if present and `null` otherwise.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (1, "One"), (2, "Two")].values(),
    Nat.compare);

  assert Map.get(map, Nat.compare, 1) == ?"One";
  assert Map.get(map, Nat.compare, 3) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `insert`
``` motoko no-repl
func insert<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : Bool
```

Given `map` ordered by `compare`, insert a new mapping from `key` to `value`.
Replaces any existing entry under `key`.
Returns true if the key is new to the map, otherwise false.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.empty<Nat, Text>();
  assert Map.insert(map, Nat.compare, 0, "Zero");
  assert Map.insert(map, Nat.compare, 1, "One");
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One")];
  assert not Map.insert(map, Nat.compare, 0, "Nil");
  assert Iter.toArray(Map.entries(map)) == [(0, "Nil"), (1, "One")]
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `add`
``` motoko no-repl
func add<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V)
```

Given `map` ordered by `compare`, add a mapping from `key` to `value` to `map`.
Replaces any existing entry for `key`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.empty<Nat, Text>();

  Map.add(map, Nat.compare, 0, "Zero");
  Map.add(map, Nat.compare, 1, "One");
  Map.add(map, Nat.compare, 0, "Nil");

  assert Iter.toArray(Map.entries(map)) == [(0, "Nil"), (1, "One")]
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `swap`
``` motoko no-repl
func swap<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : ?V
```

Associates the value with the key in the map.
If the key is not yet present in the map, a new key-value pair is added and `null` is returned.
Otherwise, if the key is already present, the value is overwritten and the previous value is returned.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.singleton<Nat, Text>(1, "One");

  assert Map.swap(map, Nat.compare, 0, "Zero") == null;
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One")];

  assert Map.swap(map, Nat.compare, 0, "Nil") == ?"Zero";
  assert Iter.toArray(Map.entries(map)) == [(0, "Nil"), (1, "One")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `replace`
``` motoko no-repl
func replace<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : ?V
```

Overwrites the value of an existing key and returns the previous value.
If the key does not exist, it has no effect and returns `null`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.singleton<Nat, Text>(0, "Zero");

  let prev1 = Map.replace(map, Nat.compare, 0, "Nil"); // overwrites the value for existing key.
  assert prev1 == ?"Zero";
  assert Map.get(map, Nat.compare, 0) == ?"Nil";

  let prev2 = Map.replace(map, Nat.compare, 1, "One");  // no effect, key is absent
  assert prev2 == null;
  assert Map.get(map, Nat.compare, 1) == null;
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `remove`
``` motoko no-repl
func remove<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K)
```

Delete an entry by its key in the map.
No effect if the key is not present.

```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (2, "Two"), (1, "One")].values(),
    Nat.compare);

  Map.remove(map, Nat.compare, 1);
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (2, "Two")];
  Map.remove(map, Nat.compare, 42);
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (2, "Two")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` including garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` objects that will be collected as garbage.

## Function `delete`
``` motoko no-repl
func delete<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Bool
```

Delete an existing entry by its key in the map.
Returns `true` if the key was present in the map, otherwise `false`.

```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (2, "Two"), (1, "One")].values(),
    Nat.compare);

  assert Map.delete(map, Nat.compare, 1); // present, returns true
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (2, "Two")];

  assert not Map.delete(map, Nat.compare, 42); // absent, returns false
  assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (2, "Two")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` including garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` objects that will be collected as garbage.

## Function `take`
``` motoko no-repl
func take<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V
```

Removes any existing entry by its key in the map.
Returns the previous value of the key or `null` if the key was absent.

```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>(
    [(0, "Zero"), (2, "Two"), (1, "One")].values(),
    Nat.compare);

  assert Map.take(map, Nat.compare, 0) == ?"Zero";
  assert Iter.toArray(Map.entries(map)) == [(1, "One"), (2, "Two")];

  assert Map.take(map, Nat.compare, 3) == null;
  assert Iter.toArray(Map.entries(map)) == [(1, "One"), (2, "Two")];
}
```

Runtime: `O(log(n))`.
Space: `O(log(n))` including garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` objects that will be collected as garbage.

## Function `maxEntry`
``` motoko no-repl
func maxEntry<K, V>(map : Map<K, V>) : ?(K, V)
```

Retrieves the key-value pair from the map with the maximum key.
If the map is empty, returns `null`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.empty<Nat, Text>();

  assert Map.maxEntry(map) == null;

  Map.add(map, Nat.compare, 0, "Zero");
  Map.add(map, Nat.compare, 2, "Two");
  Map.add(map, Nat.compare, 1, "One");

  assert Map.maxEntry(map) == ?(2, "Two")
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `minEntry`
``` motoko no-repl
func minEntry<K, V>(map : Map<K, V>) : ?(K, V)
```

Retrieves the key-value pair from the map with the minimum key.
If the map is empty, returns `null`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.empty<Nat, Text>();

  assert Map.minEntry(map) == null;

  Map.add(map, Nat.compare, 2, "Two");
  Map.add(map, Nat.compare, 0, "Zero");
  Map.add(map, Nat.compare, 1, "One");

  assert Map.minEntry(map) == ?(0, "Zero")
}
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.

## Function `entries`
``` motoko no-repl
func entries<K, V>(map : Map<K, V>) : Types.Iter<(K, V)>
```

Returns an iterator over the key-value pairs in the map,
traversing the entries in the ascending order of the keys.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

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
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `entriesFrom`
``` motoko no-repl
func entriesFrom<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Types.Iter<(K, V)>
```

Returns an iterator over the key-value pairs in the map,
starting from a given key in ascending order.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (3, "Three"),  (1, "One")].values(), Nat.compare);
  assert Iter.toArray(Map.entriesFrom(map, Nat.compare, 1)) == [(1, "One"), (3, "Three")];
  assert Iter.toArray(Map.entriesFrom(map, Nat.compare, 2)) == [(3, "Three")];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `reverseEntries`
``` motoko no-repl
func reverseEntries<K, V>(map : Map<K, V>) : Types.Iter<(K, V)>
```

Returns an iterator over the key-value pairs in the map,
traversing the entries in the descending order of the keys.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

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
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `reverseEntriesFrom`
``` motoko no-repl
func reverseEntriesFrom<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Types.Iter<(K, V)>
```

Returns an iterator over the key-value pairs in the map,
starting from a given key in descending order.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (1, "One"), (3, "Three")].values(), Nat.compare);
  assert Iter.toArray(Map.reverseEntriesFrom(map, Nat.compare, 0)) == [(0, "Zero")];
  assert Iter.toArray(Map.reverseEntriesFrom(map, Nat.compare, 2)) == [(1, "One"), (0, "Zero")];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `keys`
``` motoko no-repl
func keys<K, V>(map : Map<K, V>) : Types.Iter<K>
```

Returns an iterator over the keys in the map,
traversing all keys in ascending order.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.keys(map)) == [0, 1, 2];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)`.

## Function `values`
``` motoko no-repl
func values<K, V>(map : Map<K, V>) : Types.Iter<V>
```

Returns an iterator over the values in the map,
traversing the values in the ascending order of the keys to which they are associated.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  assert Iter.toArray(Map.values(map)) == ["Zero", "One", "Two"];
}
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(1)`.

## Function `fromIter`
``` motoko no-repl
func fromIter<K, V>(iter : Types.Iter<(K, V)>, compare : (K, K) -> Order.Order) : Map<K, V>
```

Create a mutable key-value map with the entries obtained from an iterator.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  transient let iter =
    Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]);

  let map = Map.fromIter<Nat, Text>(iter, Nat.compare);

   assert Iter.toArray(Map.entries(map)) == [(0, "Zero"), (1, "One"), (2, "Two")];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)`.
where `n` denotes the number of key-value entries returned by the iterator and
assuming that the `compare` function implements an `O(1)` comparison.

## Function `forEach`
``` motoko no-repl
func forEach<K, V>(map : Map<K, V>, operation : (K, V) -> ())
```

Apply an operation on each key-value pair contained in the map.
The operation is applied in ascending order of the keys.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
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

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `filter`
``` motoko no-repl
func filter<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, criterion : (K, V) -> Bool) : Map<K, V>
```

Filter entries in a new map.
Create a copy of the mutable map that only contains the key-value pairs
that fulfil the criterion function.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let numberNames = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

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

## Function `map`
``` motoko no-repl
func map<K, V1, V2>(map : Map<K, V1>, project : (K, V1) -> V2) : Map<K, V2>
```

Project all values of the map in a new map.
Apply a mapping function to the values of each entry in the map and
collect the mapped entries in a new mutable key-value map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func f(key : Nat, _val : Text) : Nat = key * 2;

  let resMap = Map.map<Nat, Text, Nat>(map, f);

  assert Iter.toArray(Map.entries(resMap)) == [(0, 0), (1, 2), (2, 4)];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<K, V, A>(map : Map<K, V>, base : A, combine : (A, K, V) -> A) : A
```

Iterate all entries in ascending order of the keys,
and accumulate the entries by applying the combine function, starting from a base value.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func folder(accum : (Nat, Text), key : Nat, val : Text) : ((Nat, Text))
    = (key + accum.0, accum.1 # val);

  assert Map.foldLeft(map, (0, ""), folder) == (3, "ZeroOneTwo");
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `foldRight`
``` motoko no-repl
func foldRight<K, V, A>(map : Map<K, V>, base : A, combine : (K, V, A) -> A) : A
```

Iterate all entries in descending order of the keys,
and accumulate the entries by applying the combine function, starting from a base value.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func folder(key : Nat, val : Text, accum : (Nat, Text)) : ((Nat, Text))
    = (key + accum.0, accum.1 # val);

  assert Map.foldRight(map, (0, ""), folder) == (3, "TwoOneZero");
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `all`
``` motoko no-repl
func all<K, V>(map : Map<K, V>, predicate : (K, V) -> Bool) : Bool
```

Check whether all entries in the map fulfil a predicate function, i.e.
the predicate function returns `true` for all entries in the map.
Returns `true` for an empty map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);

  assert Map.all<Nat, Text>(map, func (k, v) = v == Nat.toText(k));
  assert not Map.all<Nat, Text>(map, func (k, v) = k < 2);
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `any`
``` motoko no-repl
func any<K, V>(map : Map<K, V>, predicate : (K, V) -> Bool) : Bool
```

Test if any key-value pair in `map` satisfies the given predicate `pred`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "0"), (2, "2"), (1, "1")].values(), Nat.compare);

  assert Map.any<Nat, Text>(map, func (k, v) = (k >= 0));
  assert not Map.any<Nat, Text>(map, func (k, v) = (k >= 3));
}
```

Runtime: `O(n)`.
Space: `O(1)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `filterMap`
``` motoko no-repl
func filterMap<K, V1, V2>(map : Map<K, V1>, compare : (K, K) -> Order.Order, project : (K, V1) -> ?V2) : Map<K, V2>
```

Filter all entries in the map by also applying a projection to the value.
Apply a mapping function `project` to all entries in the map and collect all
entries, for which the function returns a non-null new value. Collect all
non-discarded entries with the key and new value in a new mutable map.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);

  func f(key : Nat, val : Text) : ?Text {
    if(key == 0) {null}
    else { ?("Twenty " # val)}
  };

  let newMap = Map.filterMap<Nat, Text, Text>(map, Nat.compare, f);

  assert Iter.toArray(Map.entries(newMap)) == [(1, "Twenty One"), (2, "Twenty Two")];
}
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `assertValid`
``` motoko no-repl
func assertValid<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order)
```

Internal sanity check function.
Can be used to check that key/value pairs have been inserted with a consistent key comparison function.
Traps if the internal map structure is invalid.

## Function `toText`
``` motoko no-repl
func toText<K, V>(map : Map<K, V>, keyFormat : K -> Text, valueFormat : V -> Text) : Text
```

Generate a textual representation of all the entries in the map.
Primarily to be used for testing and debugging.
The keys and values are formatted according to `keyFormat` and `valueFormat`.

Example:
```motoko
import Map "mo:core/Map";
import Nat "mo:core/Nat";

persistent actor {
  let map = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two"), (1, "One")].values(), Nat.compare);
  assert Map.toText<Nat, Text>(map, Nat.toText, func t { t }) == "Map{(0, Zero), (1, One), (2, Two)}";
}
```

Runtime: `O(n)`.
Space: `O(n)` retained memory plus garbage, see below.
where `n` denotes the number of key-value entries stored in the map and
assuming that `keyFormat` and `valueFormat` have runtime and space costs of `O(1)`.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.

## Function `compare`
``` motoko no-repl
func compare<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compareKey : (K, K) -> Order.Order, compareValue : (V, V) -> Order.Order) : Order.Order
```

Compare two maps by primarily comparing keys and secondarily values.
Both maps must have been created by the same key comparison function.
The two maps are iterated by the ascending order of their creation and
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
import Map "mo:core/Map";
import Nat "mo:core/Nat";
import Text "mo:core/Text";

persistent actor {
  let map1 = Map.fromIter<Nat, Text>([(0, "Zero"), (1, "One")].values(), Nat.compare);
  let map2 = Map.fromIter<Nat, Text>([(0, "Zero"), (2, "Two")].values(), Nat.compare);

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
