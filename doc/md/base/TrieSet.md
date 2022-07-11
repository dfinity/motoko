# TrieSet
Functional set

Sets are partial maps from element type to unit type,
i.e., the partial map represents the set with its domain.

## Type `Hash`
``` motoko
type Hash = Hash.Hash
```


## Type `Set`
``` motoko
type Set<T> = Trie.Trie<T, ()>
```


## Function `empty`
``` motoko
func empty<T>() : Set<T>
```

Empty set.

## Function `put`
``` motoko
func put<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T>
```

Put an element into the set.

## Function `delete`
``` motoko
func delete<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T>
```

Delete an element from the set.

## Function `equal`
``` motoko
func equal<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Bool
```

Test if two sets are equal.

## Function `size`
``` motoko
func size<T>(s : Set<T>) : Nat
```

The number of set elements, set's cardinality.

## Function `mem`
``` motoko
func mem<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Bool
```

Test if a set contains a given element.

## Function `union`
``` motoko
func union<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set union](https://en.wikipedia.org/wiki/Union_(set_theory)).

## Function `diff`
``` motoko
func diff<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set difference](https://en.wikipedia.org/wiki/Difference_(set_theory)).

## Function `intersect`
``` motoko
func intersect<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory)).

## Function `fromArray`
``` motoko
func fromArray<T>(arr : [T], elemHash : T -> Hash, eq : (T, T) -> Bool) : Set<T>
```


## Function `toArray`
``` motoko
func toArray<T>(s : Set<T>) : [T]
```

