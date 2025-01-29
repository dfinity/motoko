# AssocList
Map implemented as a linked-list of key-value pairs ("Associations").

NOTE: This map implementation is mainly used as underlying buckets for other map
structures. Thus, other map implementations are easier to use in most cases.

## Type `AssocList`
``` motoko no-repl
type AssocList<K, V> = List.List<(K, V)>
```

Import from the base library to use this module.

```motoko name=import
import AssocList "mo:base/AssocList";
import List "mo:base/List";
import Nat "mo:base/Nat";

type AssocList<K, V> = AssocList.AssocList<K, V>;
```

Initialize an empty map using an empty list.
```motoko name=initialize include=import
var map : AssocList<Nat, Nat> = List.nil(); // Empty list as an empty map
map := null; // Alternative: null as empty list.
map
```

## Function `find`
``` motoko no-repl
func find<K, V>(map : AssocList<K, V>, key : K, equal : (K, K) -> Bool) : ?V
```

Find the value associated with key `key`, or `null` if no such key exists.
Compares keys using the provided function `equal`.

Example:
```motoko include=import,initialize
// Create map = [(0, 10), (1, 11), (2, 12)]
map := AssocList.replace(map, 0, Nat.equal, ?10).0;
map := AssocList.replace(map, 1, Nat.equal, ?11).0;
map := AssocList.replace(map, 2, Nat.equal, ?12).0;

// Find value associated with key 1
AssocList.find(map, 1, Nat.equal)
```
Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `replace`
``` motoko no-repl
func replace<K, V>(map : AssocList<K, V>, key : K, equal : (K, K) -> Bool, value : ?V) : (AssocList<K, V>, ?V)
```

Maps `key` to `value` in `map`, and overwrites the old entry if the key
was already present. Returns the old value in an option if it existed and
`null` otherwise, as well as the new map. Compares keys using the provided
function `equal`.

Example:
```motoko include=import,initialize
// Add three entries to the map
// map = [(0, 10), (1, 11), (2, 12)]
map := AssocList.replace(map, 0, Nat.equal, ?10).0;
map := AssocList.replace(map, 1, Nat.equal, ?11).0;
map := AssocList.replace(map, 2, Nat.equal, ?12).0;
// Override second entry
map := AssocList.replace(map, 1, Nat.equal, ?21).0;

List.toArray(map)
```
Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `diff`
``` motoko no-repl
func diff<K, V, W>(map1 : AssocList<K, V>, map2 : AssocList<K, W>, equal : (K, K) -> Bool) : AssocList<K, V>
```

Produces a new map containing all entries from `map1` whose keys are not
contained in `map2`. The "extra" entries in `map2` are ignored. Compares
keys using the provided function `equal`.

Example:
```motoko include=import,initialize
// Create map1 = [(0, 10), (1, 11), (2, 12)]
var map1 : AssocList<Nat, Nat> = null;
map1 := AssocList.replace(map1, 0, Nat.equal, ?10).0;
map1 := AssocList.replace(map1, 1, Nat.equal, ?11).0;
map1 := AssocList.replace(map1, 2, Nat.equal, ?12).0;

// Create map2 = [(2, 12), (3, 13)]
var map2 : AssocList<Nat, Nat> = null;
map2 := AssocList.replace(map2, 2, Nat.equal, ?12).0;
map2 := AssocList.replace(map2, 3, Nat.equal, ?13).0;

// Take the difference
let newMap = AssocList.diff(map1, map2, Nat.equal);
List.toArray(newMap)
```
Runtime: O(size1 * size2)

Space: O(1)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `mapAppend`
``` motoko no-repl
func mapAppend<K, V, W, X>(map1 : AssocList<K, V>, map2 : AssocList<K, W>, f : (?V, ?W) -> X) : AssocList<K, X>
```

@deprecated

## Function `disjDisjoint`
``` motoko no-repl
func disjDisjoint<K, V, W, X>(map1 : AssocList<K, V>, map2 : AssocList<K, W>, f : (?V, ?W) -> X) : AssocList<K, X>
```

Produces a new map by mapping entries in `map1` and `map2` using `f` and
concatenating the results. Assumes that there are no collisions between
keys in `map1` and `map2`.

Example:
```motoko include=import,initialize
import { trap } "mo:base/Debug";

// Create map1 = [(0, 10), (1, 11), (2, 12)]
var map1 : AssocList<Nat, Nat> = null;
map1 := AssocList.replace(map1, 0, Nat.equal, ?10).0;
map1 := AssocList.replace(map1, 1, Nat.equal, ?11).0;
map1 := AssocList.replace(map1, 2, Nat.equal, ?12).0;

// Create map2 = [(4, "14"), (3, "13")]
var map2 : AssocList<Nat, Text> = null;
map2 := AssocList.replace(map2, 4, Nat.equal, ?"14").0;
map2 := AssocList.replace(map2, 3, Nat.equal, ?"13").0;

// Map and append the two AssocLists
let newMap =
  AssocList.disjDisjoint<Nat, Nat, Text, Text>(
    map1,
    map2,
    func((v1, v2) : (?Nat, ?Text)) {
      switch(v1, v2) {
        case(?v1, null) {
          debug_show(v1) // convert values from map1 to Text
        };
        case(null, ?v2) {
          v2 // keep values from map2 as Text
        };
        case _ {
          trap "These cases will never happen in mapAppend"
        }
      }
    }
  );

List.toArray(newMap)
```
Runtime: O(size1 + size2)

Space: O(1)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `disj`
``` motoko no-repl
func disj<K, V, W, X>(map1 : AssocList<K, V>, map2 : AssocList<K, W>, equal : (K, K) -> Bool, combine : (?V, ?W) -> X) : AssocList<K, X>
```

Creates a new map by merging entries from `map1` and `map2`, and mapping
them using `combine`. `combine` is also used to combine the values of colliding keys.
Keys are compared using the given `equal` function.

NOTE: `combine` will never be applied to `(null, null)`.

Example:
```motoko include=import,initialize
import { trap } "mo:base/Debug";

// Create map1 = [(0, 10), (1, 11), (2, 12)]
var map1 : AssocList<Nat, Nat> = null;
map1 := AssocList.replace(map1, 0, Nat.equal, ?10).0;
map1 := AssocList.replace(map1, 1, Nat.equal, ?11).0;
map1 := AssocList.replace(map1, 2, Nat.equal, ?12).0;

// Create map2 = [(2, 12), (3, 13)]
var map2 : AssocList<Nat, Nat> = null;
map2 := AssocList.replace(map2, 2, Nat.equal, ?12).0;
map2 := AssocList.replace(map2, 3, Nat.equal, ?13).0;

// Merge the two maps using `combine`
let newMap =
  AssocList.disj<Nat, Nat, Nat, Nat>(
    map1,
    map2,
    Nat.equal,
    func((v1, v2) : (?Nat, ?Nat)) : Nat {
      switch(v1, v2) {
        case(?v1, ?v2) {
          v1 + v2 // combine values of colliding keys by adding them
        };
        case(?v1, null) {
          v1 // when a key doesn't collide, keep the original value
        };
        case(null, ?v2) {
          v2
        };
        case _ {
          trap "This case will never happen in disj"
        }
      }
    }
  );

List.toArray(newMap)
```
Runtime: O(size1 * size2)

Space: O(size1 + size2)

*Runtime and space assumes that `equal` and `combine` runs in O(1) time and space.

## Function `join`
``` motoko no-repl
func join<K, V, W, X>(map1 : AssocList<K, V>, map2 : AssocList<K, W>, equal : (K, K) -> Bool, combine : (V, W) -> X) : AssocList<K, X>
```

Takes the intersection of `map1` and `map2`, only keeping colliding keys
and combining values using the `combine` function. Keys are compared using
the `equal` function.

Example:
```motoko include=import,initialize
// Create map1 = [(0, 10), (1, 11), (2, 12)]
var map1 : AssocList<Nat, Nat> = null;
map1 := AssocList.replace(map1, 0, Nat.equal, ?10).0;
map1 := AssocList.replace(map1, 1, Nat.equal, ?11).0;
map1 := AssocList.replace(map1, 2, Nat.equal, ?12).0;

// Create map2 = [(2, 12), (3, 13)]
var map2 : AssocList<Nat, Nat> = null;
map2 := AssocList.replace(map2, 2, Nat.equal, ?12).0;
map2 := AssocList.replace(map2, 3, Nat.equal, ?13).0;

// Take the intersection of the two maps, combining values by adding them
let newMap = AssocList.join<Nat, Nat, Nat, Nat>(map1, map2, Nat.equal, Nat.add);

List.toArray(newMap)
```
Runtime: O(size1 * size2)

Space: O(size1 + size2)

*Runtime and space assumes that `equal` and `combine` runs in O(1) time and space.

## Function `fold`
``` motoko no-repl
func fold<K, V, X>(map : AssocList<K, V>, base : X, combine : (K, V, X) -> X) : X
```

Collapses the elements in `map` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko include=import,initialize
// Create map = [(0, 10), (1, 11), (2, 12)]
var map : AssocList<Nat, Nat> = null;
map := AssocList.replace(map, 0, Nat.equal, ?10).0;
map := AssocList.replace(map, 1, Nat.equal, ?11).0;
map := AssocList.replace(map, 2, Nat.equal, ?12).0;

// (0 * 10) + (1 * 11) + (2 * 12)
AssocList.fold<Nat, Nat, Nat>(map, 0, func(k, v, sumSoFar) = (k * v) + sumSoFar)
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `combine` runs in O(1) time and space.
