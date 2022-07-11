# HashMap
Mutable hash map (aka Hashtable)

This module defines an imperative hash map (hash table), with a general key and value type.

It has a minimal object-oriented interface: `get`, `set`, `delete`, `count` and `entries`.

The class is parameterized by the key's equality and hash functions,
and an initial capacity.  However, as with the `Buffer` class, no array allocation
happens until the first `set`.

Internally, table growth policy is very simple, for now:
 Double the current capacity when the expected bucket list size grows beyond a certain constant.

## `class HashMap<K, V>`


### Function `size`
``` motoko norepl
func size() : Nat
```

Returns the number of entries in this HashMap.


### Function `delete`
``` motoko norepl
func delete(k : K)
```

Deletes the entry with the key `k`. Doesn't do anything if the key doesn't
exist.


### Function `remove`
``` motoko norepl
func remove(k : K) : ?V
```

Removes the entry with the key `k` and returns the associated value if it
existed or `null` otherwise.


### Function `get`
``` motoko norepl
func get(k : K) : ?V
```

Gets the entry with the key `k` and returns its associated value if it
existed or `null` otherwise.


### Function `put`
``` motoko norepl
func put(k : K, v : V)
```

Insert the value `v` at key `k`. Overwrites an existing entry with key `k`


### Function `replace`
``` motoko norepl
func replace(k : K, v : V) : ?V
```

Insert the value `v` at key `k` and returns the previous value stored at
`k` or `null` if it didn't exist.


### Function `keys`
``` motoko norepl
func keys() : Iter.Iter<K>
```

An `Iter` over the keys.


### Function `vals`
``` motoko norepl
func vals() : Iter.Iter<V>
```

An `Iter` over the values.


### Function `entries`
``` motoko norepl
func entries() : Iter.Iter<(K, V)>
```

Returns an iterator over the key value pairs in this
`HashMap`. Does _not_ modify the `HashMap`.
An imperative HashMap with a minimal object-oriented interface.
Maps keys of type `K` to values of type `V`.

## Function `clone`
``` motoko norepl
func clone<K, V>(h : HashMap<K, V>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : HashMap<K, V>
```

clone cannot be an efficient object method,
...but is still useful in tests, and beyond.

## Function `fromIter`
``` motoko norepl
func fromIter<K, V>(iter : Iter.Iter<(K, V)>, initCapacity : Nat, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash) : HashMap<K, V>
```

Clone from any iterator of key-value pairs

## Function `map`
``` motoko norepl
func map<K, V1, V2>(h : HashMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, mapFn : (K, V1) -> V2) : HashMap<K, V2>
```


## Function `mapFilter`
``` motoko norepl
func mapFilter<K, V1, V2>(h : HashMap<K, V1>, keyEq : (K, K) -> Bool, keyHash : K -> Hash.Hash, mapFn : (K, V1) -> ?V2) : HashMap<K, V2>
```

