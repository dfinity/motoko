# HashMap
Class `HashMap<K, V>` provides a hashmap from keys of type `K` to values of type `V`.
The class is parameterized by the key's equality and hash functions,
and an initial capacity.  However, the underlying allocation happens only when
the first key-value entry is inserted.

Internally, the map is represented as an array of `AssocList` (buckets).
The growth policy of the underyling array is very simple, for now: double
the current capacity when the expected bucket list size grows beyond a
certain constant.

WARNING: Certain operations are amortized O(1) time, such as `put`, but run
in worst case O(size) time. These worst case runtimes may exceed the cycles limit
per message if the size of the map is large enough. Further, this runtime analysis
assumes that the hash functions uniformly maps keys over the hash space. Grow these structures
with discretion, and with good hash functions. All amortized operations
below also list the worst case runtime.

For maps without amortization, see `TrieMap`.

Note on the constructor:
The argument `initCapacity` determines the initial number of buckets in the
underyling array. Also, the runtime and space anlyses in this documentation
assumes that the equality and hash functions for keys used to construct the
map run in O(1) time and space.

Example:
```motoko name=initialize
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";

let map = HashMap.HashMap<Text, Nat>(5, Text.equal, Text.hash);
```

Runtime: O(1)

Space: O(1)

## Class `HashMap<K, V>`

``` motoko no-repl
class HashMap<K, V>(initCapacity : Nat, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash)
```


### Function `size`
``` motoko no-repl
func size() : Nat
```

Returns the current number of key-value entries in the map.

Example:
```motoko include=initialize
map.size() // => 0
```

Runtime: O(1)

Space: O(1)


### Function `get`
``` motoko no-repl
func get(key : K) : (value : ?V)
```

Returns the value assocaited with key `key` if present and `null` otherwise.

Example:
```motoko include=initialize
map.put("key", 3);
map.get("key") // => ?3
```

Expected Runtime: O(1), Worst Case Runtime: O(size)

Space: O(1)


### Function `put`
``` motoko no-repl
func put(key : K, value : V)
```

Insert the value `value` with key `key`. Overwrites any existing entry with key `key`.

Example:
```motoko include=initialize
map.put("key", 3);
map.get("key") // => ?3
```

Expected Amortized Runtime: O(1), Worst Case Runtime: O(size)

Expected Amortized Space: O(1), Worst Case Space: O(size)

Note: If this is the first entry into this map, this operation will cause
the initial allocation of the underlying array.


### Function `replace`
``` motoko no-repl
func replace(key : K, value : V) : (oldValue : ?V)
```

Insert the value `value` with key `key`. Returns the previous value
associated with key `key` or `null` if no such value exists.

Example:
```motoko include=initialize
map.put("key", 3);
ignore map.replace("key", 2); // => ?3
map.get("key") // => ?2
```

Expected Amortized Runtime: O(1), Worst Case Runtime: O(size)

Expected Amortized Space: O(1), Worst Case Space: O(size)

Note: If this is the first entry into this map, this operation will cause
the initial allocation of the underlying array.


### Function `delete`
``` motoko no-repl
func delete(key : K)
```

Deletes the entry with the key `key`. Has no effect if `key` is not
present in the map.

Example:
```motoko include=initialize
map.put("key", 3);
map.delete("key");
map.get("key"); // => null
```

Expected Runtime: O(1), Worst Case Runtime: O(size)

Expected Space: O(1), Worst Case Space: O(size)


### Function `remove`
``` motoko no-repl
func remove(key : K) : (oldValue : ?V)
```

Deletes the entry with the key `key`. Returns the previous value
associated with key `key` or `null` if no such value exists.

Example:
```motoko include=initialize
map.put("key", 3);
map.remove("key"); // => ?3
```

Expected Runtime: O(1), Worst Case Runtime: O(size)

Expected Space: O(1), Worst Case Space: O(size)


### Function `keys`
``` motoko no-repl
func keys() : Iter.Iter<K>
```

Returns an Iterator (`Iter`) over the keys of the map.
Iterator provides a single method `next()`, which returns
keys in no specific order, or `null` when out of keys to iterate over.

Example:
```motoko include=initialize

map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

var keys = "";
for (key in map.keys()) {
  keys := key # " " # keys
};
keys // => "key3 key2 key1 "
```

Cost of iteration over all keys:

Runtime: O(size)

Space: O(1)


### Function `vals`
``` motoko no-repl
func vals() : Iter.Iter<V>
```

Returns an Iterator (`Iter`) over the values of the map.
Iterator provides a single method `next()`, which returns
values in no specific order, or `null` when out of values to iterate over.

Example:
```motoko include=initialize

map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

var sum = 0;
for (value in map.vals()) {
  sum += value;
};
sum // => 6
```

Cost of iteration over all values:

Runtime: O(size)

Space: O(1)


### Function `entries`
``` motoko no-repl
func entries() : Iter.Iter<(K, V)>
```

Returns an Iterator (`Iter`) over the key-value pairs in the map.
Iterator provides a single method `next()`, which returns
pairs in no specific order, or `null` when out of pairs to iterate over.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

var pairs = "";
for ((key, value) in map.entries()) {
  pairs := "(" # key # ", " # Nat.toText(value) # ") " # pairs
};
pairs // => "(key3, 3) (key2, 2) (key1, 1)"
```

Cost of iteration over all pairs:

Runtime: O(size)

Space: O(1)

## Function `clone`
``` motoko no-repl
func clone<K, V>(map : HashMap<K, V>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : HashMap<K, V>
```

Returns a copy of `map`, initializing the copy with the provided equality
and hash functions.

Example:
```motoko include=initialize
map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

let map2 = HashMap.clone(map, Text.equal, Text.hash);
map2.get("key1") // => ?1
```

Expected Runtime: O(size), Worst Case Runtime: O(size * size)

Expected Space: O(size), Worst Case Space: O(size)

## Function `fromIter`
``` motoko no-repl
func fromIter<K, V>(iter : Iter.Iter<(K, V)>, initCapacity : Nat, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : HashMap<K, V>
```

Returns a new map, containing all entries given by the iterator `iter`.
The new map is initialized with the provided initial capacity, equality,
and hash functions.

Example:
```motoko include=initialize
let entries = [("key3", 3), ("key2", 2), ("key1", 1)];
let iter = entries.vals();

let map2 = HashMap.fromIter<Text, Nat>(iter, entries.size(), Text.equal, Text.hash);
map2.get("key1") // => ?1
```

Expected Runtime: O(size), Worst Case Runtime: O(size * size)

Expected Space: O(size), Worst Case Space: O(size)

## Function `map`
``` motoko no-repl
func map<K, V1, V2>(hashMap : HashMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, f : (K, V1) -> V2) : HashMap<K, V2>
```

Creates a new map by applying `f` to each entry in `hashMap`. Each entry
`(k, v)` in the old map is transformed into a new entry `(k, v2)`, where
the new value `v2` is created by applying `f` to `(k, v)`.

```motoko include=initialize
map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

let map2 = HashMap.map<Text, Nat, Nat>(map, Text.equal, Text.hash, func (k, v) = v * 2);
map2.get("key2") // => ?4
```

Expected Runtime: O(size), Worst Case Runtime: O(size * size)

Expected Space: O(size), Worst Case Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<K, V1, V2>(hashMap : HashMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, f : (K, V1) -> ?V2) : HashMap<K, V2>
```

Creates a new map by applying `f` to each entry in `hashMap`. For each entry
`(k, v)` in the old map, if `f` evaluates to `null`, the entry is discarded.
Otherwise, the entry is transformed into a new entry `(k, v2)`, where
the new value `v2` is the result of applying `f` to `(k, v)`.

```motoko include=initialize
map.put("key1", 1);
map.put("key2", 2);
map.put("key3", 3);

let map2 =
  HashMap.mapFilter<Text, Nat, Nat>(
    map,
    Text.equal,
    Text.hash,
    func (k, v) = if (v == 2) { null } else { ?(v * 2)}
);
map2.get("key3") // => ?6
```

Expected Runtime: O(size), Worst Case Runtime: O(size * size)

Expected Space: O(size), Worst Case Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.
