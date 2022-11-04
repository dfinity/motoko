# TrieMap
Key-value hash maps.

An imperative hash map, with a general key and value type.

- The `class` `TrieMap` exposes the same interface as `HashMap`.

- Unlike HashMap, the internal representation uses a functional representation (via `Trie` module).

- This class does not permit a direct `clone` operation (neither does `HashMap`), but it does permit creating iterators via `iter()`.  Each iterator costs `O(1)` to create, but represents a fixed view of the mapping that does not interfere with mutations (it will _not_ reflect subsequent insertions or mutations, if any).

## `class TrieMap<K, V>`


### Function `size`
``` motoko no-repl
func size() : Nat
```

Returns the number of entries in the map.


### Function `put`
``` motoko no-repl
func put(k : K, v : V)
```

Associate a key and value, overwriting any prior association for the key.


### Function `replace`
``` motoko no-repl
func replace(k : K, v : V) : ?V
```

Put the key and value, _and_ return the (optional) prior value for the key.


### Function `get`
``` motoko no-repl
func get(k : K) : ?V
```

Get the (optional) value associated with the given key.


### Function `delete`
``` motoko no-repl
func delete(k : K)
```

Delete the (optional) value associated with the given key.


### Function `remove`
``` motoko no-repl
func remove(k : K) : ?V
```

Delete and return the (optional) value associated with the given key.


### Function `keys`
``` motoko no-repl
func keys() : I.Iter<K>
```

An `Iter` over the keys.

Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.


### Function `vals`
``` motoko no-repl
func vals() : I.Iter<V>
```

An `Iter` over the values.

Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.


### Function `entries`
``` motoko no-repl
func entries() : I.Iter<(K, V)>
```

Returns an `Iter` over the entries.

Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.

## Function `clone`
``` motoko no-repl
func clone<K, V>(h : TrieMap<K, V>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : TrieMap<K, V>
```

Clone the map, given its key operations.

## Function `fromEntries`
``` motoko no-repl
func fromEntries<K, V>(entries : I.Iter<(K, V)>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : TrieMap<K, V>
```

Clone an iterator of key-value pairs.

## Function `map`
``` motoko no-repl
func map<K, V1, V2>(h : TrieMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, mapFn : (K, V1) -> V2) : TrieMap<K, V2>
```

Transform (map) the values of a map, retaining its keys.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<K, V1, V2>(h : TrieMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, mapFn : (K, V1) -> ?V2) : TrieMap<K, V2>
```

Transform and filter the values of a map, retaining its keys.
