# OrderedMap
Stable key-value map implemented as a red-black tree with nodes storing key-value pairs.

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
type Map<K, V> = { size : Nat; root : Tree<K, V> }
```

Collection of key-value entries, ordered by the keys and key unique.
The keys have the generic type `K` and the values the generic type `V`.
If `K` and `V` is stable types then `Map<K, V>` is also stable. 
To ensure that property the `Map<K, V>` does not have any methods, instead 
they are gathered in the functor-like class `Operations` (see example there).

## Class `Operations<K>`

``` motoko no-repl
class Operations<K>(compare : (K, K) -> O.Order)
```

Class that captures key type `K` along with its ordering function `compare` 
and provides all operations to work with a map of type `Map<K, _>`.

An instance object should be created once as a canister field to ensure 
that the same ordering function is used for every operation.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";

actor {
  let natMap = Map.Make<Nat>(Nat.compare); // : Operations<Nat>
  stable var keyStorage : Map.Map<Nat, Text> = natMap.empty<Text>();
  
  public func addKey(id : Nat, key : Text) : async () {
    keyStorage := natMap.put(keyStorage, id, key);
  }
}
```

### Function `fromIter`
``` motoko no-repl
func fromIter<V>(i : I.Iter<(K, V)>) : Map<K, V>
```

Returns a new map, containing all entries given by the iterator `i`.
If there are multiple entries with the same key the last one is taken.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let m = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(Iter.toArray(natMap.entries(m))));

// [(0, "Zero"), (1, "One"), (2, "Two")]
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.


### Function `put`
``` motoko no-repl
func put<V>(m : Map<K, V>, key : K, value : V) : Map<K, V>
```

Insert the value `value` with key `key` into the map `m`. Overwrites any existing entry with key `key`.
Returns a modified map.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
var map = natMap.empty<Text>();

map := natMap.put(map, 0, "Zero");
map := natMap.put(map, 2, "Two");
map := natMap.put(map, 1, "One");

Debug.print(debug_show(Iter.toArray(natMap.entries(map))));

// [(0, "Zero"), (1, "One"), (2, "Two")]
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.
   
Note: The returned map shares with the `m` most of the tree nodes. 
Garbage collecting one of maps (e.g. after an assignment `m := natMap.put(m, k)`)
causes collecting `O(log(n))` nodes.


### Function `replace`
``` motoko no-repl
func replace<V>(m : Map<K, V>, key : K, value : V) : (Map<K, V>, ?V)
```

Insert the value `value` with key `key` into the map `m`. Returns modified map and
the previous value associated with key `key` or `null` if no such value exists.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map0 = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

let (map1, old1) = natMap.replace(map0, 0, "Nil");

Debug.print(debug_show(Iter.toArray(natMap.entries(map1))));
Debug.print(debug_show(old1));
// [(0, "Nil"), (1, "One"), (2, "Two")]
// ?"Zero"

let (map2, old2) = natMap.replace(map0, 3, "Three");

Debug.print(debug_show(Iter.toArray(natMap.entries(map2))));
Debug.print(debug_show(old2));
// [(0, "Zero"), (1, "One"), (2, "Two"), (3, "Three")]
// null
```

Runtime: `O(log(n))`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes. 
Garbage collecting one of maps (e.g. after an assignment `m := natMap.replace(m, k).0`)
causes collecting `O(log(n))` nodes.


### Function `mapFilter`
``` motoko no-repl
func mapFilter<V1, V2>(m : Map<K, V1>, f : (K, V1) -> ?V2) : Map<K, V2>
```

Creates a new map by applying `f` to each entry in the map `m`. For each entry
`(k, v)` in the old map, if `f` evaluates to `null`, the entry is discarded.
Otherwise, the entry is transformed into a new entry `(k, v2)`, where
the new value `v2` is the result of applying `f` to `(k, v)`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

func f(key : Nat, val : Text) : ?Text {
  if(key == 0) {null}
  else { ?("Twenty " # val)}
};

let newMap = natMap.mapFilter(map, f);

Debug.print(debug_show(Iter.toArray(natMap.entries(newMap))));

// [(1, "Twenty One"), (2, "Twenty Two")]
```

Runtime: `O(n * log(n))`.
Space: `O(n)` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.


### Function `get`
``` motoko no-repl
func get<V>(m : Map<K, V>, key : K) : ?V
```

Get the value associated with key `key` in the given map `m` if present and `null` otherwise.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(natMap.get(map, 1)));
Debug.print(debug_show(natMap.get(map, 42)));

// ?"One"
// null
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.


### Function `contains`
``` motoko no-repl
func contains<V>(m : Map<K, V>, key : K) : Bool
```

Test whether the map `m` contains any binding for the given `key`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show natMap.contains(map, 1)); // => true
Debug.print(debug_show natMap.contains(map, 42)); // => false
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.


### Function `maxEntry`
``` motoko no-repl
func maxEntry<V>(m : Map<K, V>) : ?(K, V)
```

Retrieves a key-value pair from the map `m` with a maximal key. If the map is empty returns `null`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(natMap.maxEntry(map))); // => ?(2, "Two")
Debug.print(debug_show(natMap.maxEntry(natMap.empty()))); // => null
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.


### Function `minEntry`
``` motoko no-repl
func minEntry<V>(m : Map<K, V>) : ?(K, V)
```

Retrieves a key-value pair from the map `m` with a minimal key. If the map is empty returns `null`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(natMap.minEntry(map))); // => ?(0, "Zero")
Debug.print(debug_show(natMap.minEntry(natMap.empty()))); // => null
```

Runtime: `O(log(n))`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.


### Function `delete`
``` motoko no-repl
func delete<V>(m : Map<K, V>, key : K) : Map<K, V>
```

Deletes the entry with the key `key` from the map `m`. Has no effect if `key` is not
present in the map. Returns modified map.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(Iter.toArray(natMap.entries(natMap.delete(map, 1)))));
Debug.print(debug_show(Iter.toArray(natMap.entries(natMap.delete(map, 42)))));

// [(0, "Zero"), (2, "Two")]
// [(0, "Zero"), (1, "One"), (2, "Two")]
```

Runtime: `O(log(n))`.
Space: `O(log(n))`
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes. 
Garbage collecting one of maps (e.g. after an assignment `m := natMap.delete(m, k).0`)
causes collecting `O(log(n))` nodes.


### Function `remove`
``` motoko no-repl
func remove<V>(m : Map<K, V>, key : K) : (Map<K, V>, ?V)
```

Deletes the entry with the key `key`. Returns modified map and the
previous value associated with key `key` or `null` if no such value exists.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map0 = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

let (map1, old1) = natMap.remove(map0, 0);

Debug.print(debug_show(Iter.toArray(natMap.entries(map1))));
Debug.print(debug_show(old1));
// [(1, "One"), (2, "Two")]
// ?"Zero"

let (map2, old2) = natMap.remove(map0, 42);

Debug.print(debug_show(Iter.toArray(natMap.entries(map2))));
Debug.print(debug_show(old2));
// [(0, "Zero"), (1, "One"), (2, "Two")]
// null
```

Runtime: `O(log(n))`.
Space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the map and
assuming that the `compare` function implements an `O(1)` comparison.

Note: The returned map shares with the `m` most of the tree nodes. 
Garbage collecting one of maps (e.g. after an assignment `m := natMap.remove(m, k)`)
causes collecting `O(log(n))` nodes.


### Function `empty`
``` motoko no-repl
func empty<V>() : Map<K, V>
```

Create a new empty map.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);

let map = natMap.empty<Text>();

Debug.print(debug_show(natMap.size(map)));

// 0
```

Cost of empty map creation
Runtime: `O(1)`.
Space: `O(1)`


### Function `entries`
``` motoko no-repl
func entries<V>(m : Map<K, V>) : I.Iter<(K, V)>
```

Returns an Iterator (`Iter`) over the key-value pairs in the map.
Iterator provides a single method `next()`, which returns
pairs in ascending order by keys, or `null` when out of pairs to iterate over.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(Iter.toArray(natMap.entries(map))));
// [(0, "Zero"), (1, "One"), (2, "Two")]
var sum = 0;
for ((k, _) in natMap.entries(map)) { sum += k; };
Debug.print(debug_show(sum)); // => 3
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `entriesRev`
``` motoko no-repl
func entriesRev<V>(m : Map<K, V>) : I.Iter<(K, V)>
```

Same as `entries` but iterates in the descending order.


### Function `keys`
``` motoko no-repl
func keys<V>(m : Map<K, V>) : I.Iter<K>
```

Returns an Iterator (`Iter`) over the keys of the map.
Iterator provides a single method `next()`, which returns
keys in ascending order, or `null` when out of keys to iterate over.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(Iter.toArray(natMap.keys(map))));

// [0, 1, 2]
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `vals`
``` motoko no-repl
func vals<V>(m : Map<K, V>) : I.Iter<V>
```

Returns an Iterator (`Iter`) over the values of the map.
Iterator provides a single method `next()`, which returns
values in ascending order of associated keys, or `null` when out of values to iterate over.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(Iter.toArray(natMap.vals(map))));

// ["Zero", "One", "Two"]
```
Cost of iteration over all elements:
Runtime: `O(n)`.
Space: `O(log(n))` retained memory plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `map`
``` motoko no-repl
func map<V1, V2>(m : Map<K, V1>, f : (K, V1) -> V2) : Map<K, V2>
```

Creates a new map by applying `f` to each entry in the map `m`. Each entry
`(k, v)` in the old map is transformed into a new entry `(k, v2)`, where
the new value `v2` is created by applying `f` to `(k, v)`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

func f(key : Nat, _val : Text) : Nat = key * 2;

let resMap = natMap.map(map, f);

Debug.print(debug_show(Iter.toArray(natMap.entries(resMap))));
// [(0, 0), (1, 2), (2, 4)]
```

Cost of mapping all the elements:
Runtime: `O(n)`.
Space: `O(n)` retained memory
where `n` denotes the number of key-value entries stored in the map.


### Function `size`
``` motoko no-repl
func size<V>(m : Map<K, V>) : Nat
```

Determine the size of the map as the number of key-value entries.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

Debug.print(debug_show(natMap.size(map)));
// 3
```

Runtime: `O(n)`.
Space: `O(1)`.


### Function `foldLeft`
``` motoko no-repl
func foldLeft<Value, Accum>(map : Map<K, Value>, base : Accum, combine : (Accum, K, Value) -> Accum) : Accum
```

Collapses the elements in the `map` into a single value by starting with `base`
and progressively combining keys and values into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

func folder(accum : (Nat, Text), key : Nat, val : Text) : ((Nat, Text))
  = (key + accum.0, accum.1 # val);

Debug.print(debug_show(natMap.foldLeft(map, (0, ""), folder)));

// (3, "ZeroOneTwo")
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `foldRight`
``` motoko no-repl
func foldRight<Value, Accum>(map : Map<K, Value>, base : Accum, combine : (K, Value, Accum) -> Accum) : Accum
```

Collapses the elements in the `map` into a single value by starting with `base`
and progressively combining keys and values into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "Zero"), (2, "Two"), (1, "One")]));

func folder(key : Nat, val : Text, accum : (Nat, Text)) : ((Nat, Text))
  = (key + accum.0, accum.1 # val);

Debug.print(debug_show(natMap.foldRight(map, (0, ""), folder)));

// (3, "TwoOneZero")
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Space: depends on `combine` function plus garbage, see the note below.
where `n` denotes the number of key-value entries stored in the map.

Note: Full map iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `all`
``` motoko no-repl
func all<V>(m : Map<K, V>, pred : (K, V) -> Bool) : Bool
```

Test whether all key-value pairs satisfy a given predicate `pred`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "0"), (2, "2"), (1, "1")]));

Debug.print(debug_show(natMap.all<Text>(map, func (k, v) = (v == debug_show(k)))));
// true
Debug.print(debug_show(natMap.all<Text>(map, func (k, v) = (k < 2))));
// false
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.


### Function `some`
``` motoko no-repl
func some<V>(m : Map<K, V>, pred : (K, V) -> Bool) : Bool
```

Test if there exists a key-value pair satisfying a given predicate `pred`.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

let natMap = Map.Make<Nat>(Nat.compare);
let map = natMap.fromIter<Text>(Iter.fromArray([(0, "0"), (2, "2"), (1, "1")]));

Debug.print(debug_show(natMap.some<Text>(map, func (k, v) = (k >= 3))));
// false
Debug.print(debug_show(natMap.some<Text>(map, func (k, v) = (k >= 0))));
// true
```

Runtime: `O(n)`.
Space: `O(1)`.
where `n` denotes the number of key-value entries stored in the map.


### Function `validate`
``` motoko no-repl
func validate<V>(m : Map<K, V>) : ()
```

Debug helper that check internal invariants of the given map `m`. 
Raise an error (for a stack trace) if invariants are violated.

## Value `Make`
``` motoko no-repl
let Make : <K>(compare : (K, K) -> O.Order) -> Operations<K>
```

Create `OrderedMap.Operations` object capturing key type `K` and `compare` function. 
It is an alias for the `Operations` constructor.

Example:
```motoko
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";

actor {
  let natMap = Map.Make<Nat>(Nat.compare);
  stable var map : Map.Map<Nat, Text> = natMap.empty<Text>();
};
```
