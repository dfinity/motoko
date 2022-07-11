# Trie
Functional key-value hash maps.

Functional maps (and sets) whose representation is "canonical", and
independent of operation history (unlike other popular search trees).

The representation we use here comes from Section 6 of ["Incremental computation via function caching", Pugh & Teitelbaum](https://dl.acm.org/citation.cfm?id=75305).

## User's overview

This module provides an applicative (functional) hash map.
Notably, each `put` produces a **new trie _and value being replaced, if any_**.

Those looking for a more familiar (imperative,
object-oriented) hash map should consider `TrieMap` or `HashMap` instead.

The basic `Trie` operations consist of:
- `put` - put a key-value into the trie, producing a new version.
- `get` - get a key's value from the trie, or `null` if none.
- `iter` - visit every key-value in the trie.

The `put` and `get` operations work over `Key` records,
which group the hash of the key with its non-hash key value.

```motoko
import Trie "mo:base/Trie";
import Text "mo:base/Text";

type Trie<K, V> = Trie.Trie<K, V>;
type Key<K> = Trie.Key<K>;

func key(t: Text) : Key<Text> { { key = t; hash = Text.hash t } };

let t0 : Trie<Text, Nat> = Trie.empty();
let t1 : Trie<Text, Nat> = Trie.put(t0, key "hello", Text.equal, 42).0;
let t2 : Trie<Text, Nat> = Trie.put(t1, key "world", Text.equal, 24).0;
let n : ?Nat = Trie.put(t1, key "hello", Text.equal, 0).1;
assert (n == ?42);
```


## Type `Trie`
`type Trie<K, V> = {#empty; #leaf : Leaf<K, V>; #branch : Branch<K, V>}`

Binary hash tries: either empty, a leaf node, or a branch node

## Type `Leaf`
`type Leaf<K, V> = { size : Nat; keyvals : AssocList<Key<K>, V> }`

Leaf nodes of trie consist of key-value pairs as a list.

## Type `Branch`
`type Branch<K, V> = { size : Nat; left : Trie<K, V>; right : Trie<K, V> }`

Branch nodes of the trie discriminate on a bit position of the keys' hashes.
we never store this bitpos; rather,
we enforce a style where this position is always known from context.

## Type `AssocList`
`type AssocList<K, V> = AssocList.AssocList<K, V>`


## Type `Key`
`type Key<K> = { hash : Hash.Hash; key : K }`


## Function `equalKey`
`func equalKey<K>(keq : (K, K) -> Bool) : ((Key<K>, Key<K>) -> Bool)`

Equality function for two `Key<K>`s, in terms of equality of `K`'s.

## Function `isValid`
`func isValid<K, V>(t : Trie<K, V>, enforceNormal : Bool) : Bool`

Checks the invariants of the trie structure, including the placement of keys at trie paths

## Type `Trie2D`
`type Trie2D<K1, K2, V> = Trie<K1, Trie<K2, V>>`

A 2D trie maps dimension-1 keys to another
layer of tries, each keyed on the dimension-2 keys.

## Type `Trie3D`
`type Trie3D<K1, K2, K3, V> = Trie<K1, Trie2D<K2, K3, V>>`

A 3D trie maps dimension-1 keys to another
layer of 2D tries, each keyed on the dimension-2 and dimension-3 keys.

## Function `empty`
`func empty<K, V>() : Trie<K, V>`

An empty trie.

## Function `size`
`func size<K, V>(t : Trie<K, V>) : Nat`

 Get the number of key-value pairs in the trie, in constant time.
Get size in O(1) time.

## Function `branch`
`func branch<K, V>(l : Trie<K, V>, r : Trie<K, V>) : Trie<K, V>`

Construct a branch node, computing the size stored there.

## Function `leaf`
`func leaf<K, V>(kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V>`

Construct a leaf node, computing the size stored there.

This helper function automatically enforces the MAX_LEAF_SIZE
by constructing branches as necessary; to do so, it also needs the bitpos
of the leaf.

## Function `fromList`
`func fromList<K, V>(kvc : ?Nat, kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V>`

Transform a list into a trie, splitting input list into small (leaf) lists, if necessary.

## Function `clone`
`func clone<K, V>(t : Trie<K, V>) : Trie<K, V>`

Clone the trie efficiently, via sharing.

Purely-functional representation permits _O(1)_ copy, via persistent sharing.

## Function `replace`
`func replace<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : ?V) : (Trie<K, V>, ?V)`

Replace the given key's value option with the given one, returning the previous one

## Function `put`
`func put<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : V) : (Trie<K, V>, ?V)`

Put the given key's value in the trie; return the new trie, and the previous value associated with the key, if any

## Function `get`
`func get<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V`

Get the value of the given key in the trie, or return null if nonexistent

## Function `find`
`func find<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V`

Find the given key's value in the trie, or return null if nonexistent

## Function `merge`
`func merge<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V>`

Merge tries, preferring the right trie where there are collisions
in common keys.

note: the `disj` operation generalizes this `merge`
operation in various ways, and does not (in general) lose
information; this operation is a simpler, special case.

## Function `mergeDisjoint`
`func mergeDisjoint<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V>`

Merge tries like `merge`, except signals a
dynamic error if there are collisions in common keys between the
left and right inputs.

## Function `diff`
`func diff<K, V, W>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool) : Trie<K, V>`

Difference of tries. The output consists are pairs of
the left trie whose keys are not present in the right trie; the
values of the right trie are irrelevant.

## Function `disj`
`func disj<K, V, W, X>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool, vbin : (?V, ?W) -> X) : Trie<K, X>`

Map disjunction.

This operation generalizes the notion of "set union" to finite maps.

Produces a "disjunctive image" of the two tries, where the values of
matching keys are combined with the given binary operator.

For unmatched key-value pairs, the operator is still applied to
create the value in the image.  To accomodate these various
situations, the operator accepts optional values, but is never
applied to (null, null).

Implements the database idea of an ["outer join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).


## Function `join`
`func join<K, V, W, X>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool, vbin : (V, W) -> X) : Trie<K, X>`

Map join.

Implements the database idea of an ["inner join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).

This operation generalizes the notion of "set intersection" to
finite maps.  The values of matching keys are combined with the given binary
operator, and unmatched key-value pairs are not present in the output.


## Function `foldUp`
`func foldUp<K, V, X>(t : Trie<K, V>, bin : (X, X) -> X, leaf : (K, V) -> X, empty : X) : X`

This operation gives a recursor for the internal structure of
tries.  Many common operations are instantiations of this function,
either as clients, or as hand-specialized versions (e.g., see , map,
mapFilter, some and all below).

## Function `prod`
`func prod<K1, V1, K2, V2, K3, V3>(tl : Trie<K1, V1>, tr : Trie<K2, V2>, op : (K1, V1, K2, V2) -> ?(Key<K3>, V3), k3_eq : (K3, K3) -> Bool) : Trie<K3, V3>`

Map product.

Conditional _catesian product_, where the given
operation `op` _conditionally_ creates output elements in the
resulting trie.

The keyed structure of the input tries are not relevant for this
operation: all pairs are considered, regardless of keys matching or
not.  Moreover, the resulting trie may use keys that are unrelated to
these input keys.


## Function `iter`
`func iter<K, V>(t : Trie<K, V>) : I.Iter<(K, V)>`

Returns an `Iter` over the key-value entries of the trie.

Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.

## Value `Build`
`let Build`

Represent the construction of tries as data.

This module provides optimized variants of normal tries, for
more efficient join queries.

The central insight is that for (unmaterialized) join query results, we
do not need to actually build any resulting trie of the resulting
data, but rather, just need a collection of what would be in that
trie.  Since query results can be large (quadratic in the DB size),
avoiding the construction of this trie provides a considerable savings.

To get this savings, we use an ADT for the operations that _would_ build this trie,
if evaluated. This structure specializes a rope: a balanced tree representing a
sequence.  It is only as balanced as the tries from which we generate
these build ASTs.  They have no intrinsic balance properties of their
own.


## Function `fold`
`func fold<K, V, X>(t : Trie<K, V>, f : (K, V, X) -> X, x : X) : X`

Fold over the key-value pairs of the trie, using an accumulator.
The key-value pairs have no reliable or meaningful ordering.

## Function `some`
`func some<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool`

Test whether a given key-value pair is present, or not.

## Function `all`
`func all<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool`

Test whether all key-value pairs have a given property.

## Function `nth`
`func nth<K, V>(t : Trie<K, V>, i : Nat) : ?(Key<K>, V)`

Project the nth key-value pair from the trie.

Note: This position is not meaningful; it's only here so that we
can inject tries into arrays using functions like `Array.tabulate`.

## Function `toArray`
`func toArray<K, V, W>(t : Trie<K, V>, f : (K, V) -> W) : [W]`

Gather the collection of key-value pairs into an array of a (possibly-distinct) type.

## Function `isEmpty`
`func isEmpty<K, V>(t : Trie<K, V>) : Bool`

Test for "deep emptiness": subtrees that have branching structure,
but no leaves.  These can result from naive filtering operations;
filter uses this function to avoid creating such subtrees.

## Function `filter`
`func filter<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Trie<K, V>`

Filter the key-value pairs by a given predicate.

## Function `mapFilter`
`func mapFilter<K, V, W>(t : Trie<K, V>, f : (K, V) -> ?W) : Trie<K, W>`

Map and filter the key-value pairs by a given predicate.

## Function `equalStructure`
`func equalStructure<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, keq : (K, K) -> Bool, veq : (V, V) -> Bool) : Bool`

Test for equality, but naively, based on structure.
Does not attempt to remove "junk" in the tree;
For instance, a "smarter" approach would equate
  `#bin {left = #empty; right = #empty}`
with
  `#empty`.
We do not observe that equality here.

## Function `replaceThen`
`func replaceThen<K, V, X>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v2 : V, success : (Trie<K, V>, V) -> X, fail : () -> X) : X`

Replace the given key's value in the trie,
and only if successful, do the success continuation,
otherwise, return the failure value

## Function `putFresh`
`func putFresh<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : V) : Trie<K, V>`

Put the given key's value in the trie; return the new trie; assert that no prior value is associated with the key

## Function `put2D`
`func put2D<K1, K2, V>(t : Trie2D<K1, K2, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, v : V) : Trie2D<K1, K2, V>`

Put the given key's value in the 2D trie; return the new 2D trie.

## Function `put3D`
`func put3D<K1, K2, K3, V>(t : Trie3D<K1, K2, K3, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, k3 : Key<K3>, k3_eq : (K3, K3) -> Bool, v : V) : Trie3D<K1, K2, K3, V>`

Put the given key's value in the trie; return the new trie;

## Function `remove`
`func remove<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : (Trie<K, V>, ?V)`

Remove the given key's value in the trie; return the new trie

## Function `removeThen`
`func removeThen<K, V, X>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, success : (Trie<K, V>, V) -> X, fail : () -> X) : X`

Remove the given key's value in the trie,
and only if successful, do the success continuation,
otherwise, return the failure value

## Function `remove2D`
`func remove2D<K1, K2, V>(t : Trie2D<K1, K2, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool) : (Trie2D<K1, K2, V>, ?V)`

remove the given key-key pair's value in the 2D trie; return the
new trie, and the prior value, if any.

## Function `remove3D`
`func remove3D<K1, K2, K3, V>(t : Trie3D<K1, K2, K3, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, k3 : Key<K3>, k3_eq : (K3, K3) -> Bool) : (Trie3D<K1, K2, K3, V>, ?V)`

Remove the given key-key pair's value in the 3D trie; return the
new trie, and the prior value, if any.

## Function `mergeDisjoint2D`
`func mergeDisjoint2D<K1, K2, V>(t : Trie2D<K1, K2, V>, k1_eq : (K1, K1) -> Bool, k2_eq : (K2, K2) -> Bool) : Trie<K2, V>`

Like [`mergeDisjoint`](#mergedisjoint), except instead of merging a
pair, it merges the collection of dimension-2 sub-trees of a 2D
trie.
