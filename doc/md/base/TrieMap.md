# TrieMap
Class `TrieMap<K, V>` provides a map from keys of type `K` to values of type `V`.
The class wraps and manipulates an underyling hash trie, found in the `Trie`
module. The trie is a binary tree in which the position of elements in the
tree are determined using the hash of the elements.

LIMITATIONS: This data structure allows at most MAX_LEAF_SIZE=8 hash collisions:
attempts to insert more than MAX_LEAF_SIZE keys (whether directly via `put` or indirectly via other operations) with the same hash value will trap.
This limitation is inherited from the underlying `Trie` data structure.


Note: The `class` `TrieMap` exposes the same interface as `HashMap`.

Creating a map:
The equality function is used to compare keys, and the hash function is used
to hash keys. See the example below.

```motoko name=initialize
import TrieMap "mo:base/TrieMap";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";

let map = TrieMap.TrieMap<Nat, Nat>(Nat.equal, Hash.hash)
```

## Class `TrieMap<K, V>`

``` motoko no-repl
class TrieMap<K, V>(isEq : (K, K) -> Bool, hashOf : K -> Hash.Hash)
```


### Function `size`
``` motoko no-repl
func size() : Nat
```

Returns the number of entries in the map.

Example:
```motoko include=initialize
map.size()
```

Runtime: O(1)
Space: O(1)


### Function `put`
``` motoko no-repl
func put(key : K, value : V)
```

Maps `key` to `value`, and overwrites the old entry if the key
was already present.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(2, 12);
Iter.toArray(map.entries())
```

Runtime: O(log(size))
Space: O(log(size))

*Runtime and space assumes that the trie is reasonably balanced and the
map is using a constant time and space equality and hash function.


### Function `replace`
``` motoko no-repl
func replace(key : K, value : V) : ?V
```

Maps `key` to `value`. Overwrites _and_ returns the old entry as an
option if the key was already present, and `null` otherwise.

Example:
```motoko include=initialize
map.put(0, 10);
map.replace(0, 20)
```

Runtime: O(log(size))
Space: O(log(size))

*Runtime and space assumes that the trie is reasonably balanced and the
map is using a constant time and space equality and hash function.


### Function `get`
``` motoko no-repl
func get(key : K) : ?V
```

Gets the value associated with the key `key` in an option, or `null` if it
doesn't exist.

Example:
```motoko include=initialize
map.put(0, 10);
map.get(0)
```

Runtime: O(log(size))
Space: O(log(size))

*Runtime and space assumes that the trie is reasonably balanced and the
map is using a constant time and space equality and hash function.


### Function `delete`
``` motoko no-repl
func delete(key : K)
```

Delete the entry associated with key `key`, if it exists. If the key is
absent, there is no effect.

Note: The deletion of an existing key shrinks the trie map.

Example:
```motoko include=initialize
map.put(0, 10);
map.delete(0);
map.get(0)
```

Runtime: O(log(size))
Space: O(log(size))

*Runtime and space assumes that the trie is reasonably balanced and the
map is using a constant time and space equality and hash function.


### Function `remove`
``` motoko no-repl
func remove(key : K) : ?V
```

Delete the entry associated with key `key`. Return the deleted value
as an option if it exists, and `null` otherwise.

Note: The deletion of an existing key shrinks the trie map.

Example:
```motoko include=initialize
map.put(0, 10);
map.remove(0)
```

Runtime: O(log(size))
Space: O(log(size))

*Runtime and space assumes that the trie is reasonably balanced and the
map is using a constant time and space equality and hash function.


### Function `keys`
``` motoko no-repl
func keys() : I.Iter<K>
```

Returns an iterator over the keys of the map.

Each iterator gets a _snapshot view_ of the mapping, and is unaffected
by concurrent updates to the iterated map.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);

// find the sum of all the keys
var sum = 0;
for (key in map.keys()) {
  sum += key;
};
// 0 + 1 + 2
sum
```

Runtime: O(1)
Space: O(1)

*The above runtime and space are for the construction of the iterator.
The iteration itself takes linear time and logarithmic space to execute.


### Function `vals`
``` motoko no-repl
func vals() : I.Iter<V>
```

Returns an iterator over the values in the map.

Each iterator gets a _snapshot view_ of the mapping, and is unaffected
by concurrent updates to the iterated map.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);

// find the sum of all the values
var sum = 0;
for (key in map.vals()) {
  sum += key;
};
// 10 + 11 + 12
sum
```

Runtime: O(1)
Space: O(1)

*The above runtime and space are for the construction of the iterator.
The iteration itself takes linear time and logarithmic space to execute.


### Function `entries`
``` motoko no-repl
func entries() : I.Iter<(K, V)>
```

Returns an iterator over the entries (key-value pairs) in the map.

Each iterator gets a _snapshot view_ of the mapping, and is unaffected
by concurrent updates to the iterated map.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);

// find the sum of all the products of key-value pairs
var sum = 0;
for ((key, value) in map.entries()) {
  sum += key * value;
};
// (0 * 10) + (1 * 11) + (2 * 12)
sum
```

Runtime: O(1)
Space: O(1)

*The above runtime and space are for the construction of the iterator.
The iteration itself takes linear time and logarithmic space to execute.

## Function `clone`
``` motoko no-repl
func clone<K, V>(map : TrieMap<K, V>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : TrieMap<K, V>
```

Produce a copy of `map`, using `keyEq` to compare keys and `keyHash` to
hash keys.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);
// Clone using the same equality and hash functions used to initialize `map`
let mapCopy = TrieMap.clone(map, Nat.equal, Hash.hash);
Iter.toArray(mapCopy.entries())
```

Runtime: O(size * log(size))
Space: O(size)

*Runtime and space assumes that the trie underlying `map` is reasonably
balanced and that `keyEq` and `keyHash` run in O(1) time and space.

## Function `fromEntries`
``` motoko no-repl
func fromEntries<K, V>(entries : I.Iter<(K, V)>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : TrieMap<K, V>
```

Create a new map from the entries in `entries`, using `keyEq` to compare
keys and `keyHash` to hash keys.

Example:
```motoko include=initialize
let entries = [(0, 10), (1, 11), (2, 12)];
let newMap = TrieMap.fromEntries<Nat, Nat>(entries.vals(), Nat.equal, Hash.hash);
newMap.get(2)
```

Runtime: O(size * log(size))
Space: O(size)

*Runtime and space assumes that `entries` returns elements in O(1) time,
and `keyEq` and `keyHash` run in O(1) time and space.

## Function `map`
``` motoko no-repl
func map<K, V1, V2>(map : TrieMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, f : (K, V1) -> V2) : TrieMap<K, V2>
```

Transform (map) the values in `map` using function `f`, retaining the keys.
Uses `keyEq` to compare keys and `keyHash` to hash keys.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);
// double all the values in map
let newMap = TrieMap.map<Nat, Nat, Nat>(map, Nat.equal, Hash.hash, func(key, value) = value * 2);
Iter.toArray(newMap.entries())
```

Runtime: O(size * log(size))
Space: O(size)

*Runtime and space assumes that `f`, `keyEq`, and `keyHash` run in O(1)
time and space.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<K, V1, V2>(map : TrieMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, f : (K, V1) -> ?V2) : TrieMap<K, V2>
```

Transform (map) the values in `map` using function `f`, discarding entries
for which `f` evaluates to `null`. Uses `keyEq` to compare keys and
`keyHash` to hash keys.

Example:
```motoko include=initialize
map.put(0, 10);
map.put(1, 11);
map.put(2, 12);
// double all the values in map, only keeping entries that have an even key
let newMap =
  TrieMap.mapFilter<Nat, Nat, Nat>(
    map,
    Nat.equal,
    Hash.hash,
    func(key, value) = if (key % 2 == 0) { ?(value * 2) } else { null }
  );
Iter.toArray(newMap.entries())
```

Runtime: O(size * log(size))
Space: O(size)

*Runtime and space assumes that `f`, `keyEq`, and `keyHash` run in O(1)
time and space.
