# base/HashMap
Class `HashMap<K, V>` provides a hashmap from keys of type `K` to values of type `V`.
The class is parameterized by the key's equality and hash functions, and an initial capacity.
However, the underlying allocation occurs only upon the first insertion.

Internally, the map is backed by an array of `AssocList` (buckets).
The array doubles in size when the expected bucket list size grows beyond a fixed threshold.

:::warning Performance considerations

Certain operations, such as `put`, are amortized `O(1)` but can run in worst-case `O(size)` time.
These worst cases may exceed the cycle limit per message on large maps.
This analysis assumes that the hash function distributes keys uniformly.
Use caution when growing large maps and ensure good hash functions are used.

:::

:::note Non-amortized alternative

For maps without amortization, see `TrieMap`.
:::

:::info Constructor note

The `initCapacity` argument sets the initial number of buckets.
All runtime and space complexities assume that the equality and hash functions run in `O(1)` time and space.

:::

Example:

```motoko name=initialize
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";

let map = HashMap.HashMap<Text, Nat>(5, Text.equal, Text.hash);
```

| Runtime   | Space     |
|-----------|-----------|
| `O(1)` | `O(1)` |

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

| Runtime   | Space     |
|-----------|-----------|
| `O(1)` | `O(1)` |


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

| Runtime(worst) | Runtime(amortized) |  Space |
|----------------------------|--------------------|---------------------------|
| `O(size)`                     | `O(1)`          | `O(1)`                    |



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

| Runtime(amortized) | Runtime(worst) | Space (amortized) | Space(worst)
|----------------------------|--------------------|---------------------------|------------------|
| `O(1)`                     | `O(size)`          | `O(1)`                    | `O(size)`        |
:::note Initial allocation

This operation triggers the allocation of the underlying array if it is the first entry in the map.
:::


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

| Expected Amortized Runtime | Worst Case Runtime | Expected Amortized Space | Worst Case Space |
|----------------------------|--------------------|---------------------------|------------------|
| `O(1)`                     | `O(size)`          | `O(1)`                    | `O(size)`        |

:::note Initial allocation

This operation triggers the allocation of the underlying array if it is the first entry in the map.
:::


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

| Expected Runtime | Worst Case Runtime | Expected Space | Worst Case Space |
|------------------|--------------------|----------------|------------------|
| `O(1)`           | `O(size)`          | `O(1)`         | `O(size)`        |



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

| Expected Runtime | Worst Case Runtime | Expected Space | Worst Case Space |
|------------------|--------------------|----------------|------------------|
| `O(1)`           | `O(size)`          | `O(1)`         | `O(size)`        |


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

| Runtime   | Space     |
|-----------|-----------|
| `O(size)` | `O(1)` |


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

| Runtime   | Space     |
|-----------|-----------|
| `O(size)` | `O(1)` |


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

| Runtime   | Space     |
|-----------|-----------|
| `O(size)` | `O(1)` |

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

| Runtime(expected) | Runtime(worst) |  Space(expected) | Space(worst) |
|------------------|--------------------|----------------|------------------|
| `O(size)`        | `O(size * size)`   | `O(size)`      | `O(size)`        |

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

| Runtime(expected) | Runtime(worst) |  Space(expected) | Space(worst) |
|------------------|--------------------|----------------|------------------|
| `O(size)`        | `O(size * size)`   | `O(size)`      | `O(size)`        |

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

| Runtime(expected) | Runtime(worst) |  Space(expected) | Space(worst) |
|------------------|--------------------|----------------|------------------|
| `O(size)`        | `O(size * size)`   | `O(size)`      | `O(size)`        |

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

| Runtime(expected) | Runtime(worst) |  Space(expected) | Space(worst) |
|------------------|--------------------|----------------|------------------|
| `O(size)`        | `O(size * size)`   | `O(size)`      | `O(size)`        |
