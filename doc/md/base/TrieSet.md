# TrieSet
Functional set

Sets are partial maps from element type to unit type,
i.e., the partial map represents the set with its domain.

## Type `Hash`
``` motoko norepl
type Hash = Hash.Hash
```


## Type `Set`
``` motoko norepl
type Set<T> = Trie.Trie<T, ()>
```


## Function `empty`
``` motoko norepl
func empty<T>() : Set<T>
```

Empty set.

## Function `put`
``` motoko norepl
func put<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T>
```

Put an element into the set.

## Function `delete`
``` motoko norepl
func delete<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Set<T>
```

Delete an element from the set.

## Function `equal`
``` motoko norepl
func equal<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Bool
```

Test if two sets are equal.

## Function `size`
``` motoko norepl
func size<T>(s : Set<T>) : Nat
```

The number of set elements, set's cardinality.

## Function `mem`
``` motoko norepl
func mem<T>(s : Set<T>, x : T, xh : Hash, eq : (T, T) -> Bool) : Bool
```

Test if a set contains a given element.

## Function `union`
``` motoko norepl
func union<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set union](https://en.wikipedia.org/wiki/Union_(set_theory)).

## Function `diff`
``` motoko norepl
func diff<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set difference](https://en.wikipedia.org/wiki/Difference_(set_theory)).

## Function `intersect`
``` motoko norepl
func intersect<T>(s1 : Set<T>, s2 : Set<T>, eq : (T, T) -> Bool) : Set<T>
```

[Set intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory)).

## Function `fromArray`
``` motoko norepl
func fromArray<T>(arr : [T], elemHash : T -> Hash, eq : (T, T) -> Bool) : Set<T>
```


## Function `toArray`
``` motoko norepl
func toArray<T>(s : Set<T>) : [T]
```

