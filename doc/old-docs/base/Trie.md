# Trie
Functional key-value hash maps.

This module provides an applicative (functional) hash map, called a trie.
Notably, each operation produces a new trie rather than destructively updating an existing trie.

Those looking for a more familiar (imperative,
object-oriented) hash map should consider `TrieMap` or `HashMap` instead.

The basic `Trie` operations consist of:
- `put` - put a key-value into the trie, producing a new version.
- `get` - get a key's value from the trie, or `null` if none.
- `remove` - remove a key's value from the trie
- `iter` - visit every key-value in the trie.

The `put`, `get` and `remove` operations work over `Key` records,
which group the hash of the key with its non-hash key value.

LIMITATIONS: This data structure allows at most MAX_LEAF_SIZE=8 hash collisions:
attempts to insert more than MAX_LEAF_SIZE keys (whether directly via `put` or indirectly via other operations) with the same hash value will trap.

CREDITS: Based on Section 6 of ["Incremental computation via function caching", Pugh & Teitelbaum](https://dl.acm.org/citation.cfm?id=75305).


Example:
```motoko
import Trie "mo:base/Trie";
import Text "mo:base/Text";

// we do this to have shorter type names and thus
// better readibility
type Trie<K, V> = Trie.Trie<K, V>;
type Key<K> = Trie.Key<K>;

// we have to provide `put`, `get` and `remove` with
// a record of type `Key<K> = { hash : Hash.Hash; key : K }`;
// thus we define the following function that takes a value of type `K`
// (in this case `Text`) and returns a `Key<K>` record.
func key(t: Text) : Key<Text> { { hash = Text.hash t; key = t } };

// we start off by creating an empty `Trie`
let t0 : Trie<Text, Nat> = Trie.empty();

// `put` requires 4 arguments:
// - the trie we want to insert the value into,
// - the key of the value we want to insert (note that we use the `key` function defined above),
// - a function that checks for equality of keys, and
// - the value we want to insert.
//
// When inserting a value, `put` returns a tuple of type `(Trie<K, V>, ?V)`.
// to get the new trie that contains the value,  we use the `0` projection
// and assign it to `t1` and `t2` respectively.
let t1 : Trie<Text, Nat> = Trie.put(t0, key "hello", Text.equal, 42).0;
let t2 : Trie<Text, Nat> = Trie.put(t1, key "world", Text.equal, 24).0;

// If for a given key there already was a value in the trie, `put` returns
// that previous value as the second element of the tuple.
// in our case we have already inserted the value 42 for the key "hello", so
// `put` returns 42 as the second element of the tuple.
let (t3, n) : (Trie<Text, Nat>, ?Nat) = Trie.put(
  t2,
  key "hello",
  Text.equal,
  0,
);
assert (n == ?42);

// `get` requires 3 arguments:
// - the trie we want to get the value from
// - the key of the value we want to get (note that we use the `key` function defined above)
// - a function that checks for equality of keys
//
// If the given key is nonexistent in the trie, `get` returns `null`.
var value = Trie.get(t3, key "hello", Text.equal); // Returns `?42`
assert(value == ?0);
value := Trie.get(t3, key "universe", Text.equal); // Returns `null`
assert(value == null);

// `remove` requires 3 arguments:
// - the trie we want to remove the value from,
// - the key of the value we want to remove (note that we use the `key` function defined above), and
// - a function that checks for equality of keys.
//
// In the case of keys of type `Text`, we can use `Text.equal`
// to check for equality of keys. Function `remove` returns a tuple of type `(Trie<K, V>, ?V)`.
// where the second element of the tuple is the value that was removed, or `null` if
// there was no value for the given key.
let removedValue : ?Nat = Trie.remove(
  t3,
  key "hello",
  Text.equal,
).1;
assert (removedValue == ?0);

// To iterate over the Trie, we use the `iter` function that takes a trie
// of type `Trie<K,V>` and returns an iterator of type `Iter<(K,V)>`:
var sum : Nat = 0;
for (kv in Trie.iter(t3)) {
  sum += kv.1;
};
assert(sum == 24);
```

## Type `Trie`
``` motoko no-repl
type Trie<K, V> = {#empty; #leaf : Leaf<K, V>; #branch : Branch<K, V>}
```

Binary hash tries: either empty, a leaf node, or a branch node

## Type `Leaf`
``` motoko no-repl
type Leaf<K, V> = { size : Nat; keyvals : AssocList<Key<K>, V> }
```

Leaf nodes of trie consist of key-value pairs as a list.

## Type `Branch`
``` motoko no-repl
type Branch<K, V> = { size : Nat; left : Trie<K, V>; right : Trie<K, V> }
```

Branch nodes of the trie discriminate on a bit position of the keys' hashes.
This bit position is not stored in the branch but determined from
the context of the branch.

## Type `AssocList`
``` motoko no-repl
type AssocList<K, V> = AssocList.AssocList<K, V>
```


## Type `Key`
``` motoko no-repl
type Key<K> = { hash : Hash.Hash; key : K }
```

A `Key` for the trie has an associated hash value
- `hash` permits fast inequality checks, and permits collisions, while
- `key` permits precise equality checks, but is only used on values with equal hashes.

## Function `equalKey`
``` motoko no-repl
func equalKey<K>(keq : (K, K) -> Bool) : ((Key<K>, Key<K>) -> Bool)
```

Equality function for two `Key<K>`s, in terms of equality of `K`'s.

## Function `isValid`
``` motoko no-repl
func isValid<K, V>(t : Trie<K, V>, _enforceNormal : Bool) : Bool
```

@deprecated `isValid` is an internal predicate and will be removed in future.

## Type `Trie2D`
``` motoko no-repl
type Trie2D<K1, K2, V> = Trie<K1, Trie<K2, V>>
```

A 2D trie maps dimension-1 keys to another
layer of tries, each keyed on the dimension-2 keys.

## Type `Trie3D`
``` motoko no-repl
type Trie3D<K1, K2, K3, V> = Trie<K1, Trie2D<K2, K3, V>>
```

A 3D trie maps dimension-1 keys to another
Composition of 2D tries, each keyed on the dimension-2 and dimension-3 keys.

## Function `empty`
``` motoko no-repl
func empty<K, V>() : Trie<K, V>
```

An empty trie. This is usually the starting point for building a trie.

Example:
```motoko name=initialize
import { print } "mo:base/Debug";
import Trie "mo:base/Trie";
import Text "mo:base/Text";

// we do this to have shorter type names and thus
// better readibility
type Trie<K, V> = Trie.Trie<K, V>;
type Key<K> = Trie.Key<K>;

// We have to provide `put`, `get` and `remove` with
// a function of return type `Key<K> = { hash : Hash.Hash; key : K }`
func key(t: Text) : Key<Text> { { hash = Text.hash t; key = t } };
// We start off by creating an empty `Trie`
var trie : Trie<Text, Nat> = Trie.empty();
```

## Function `size`
``` motoko no-repl
func size<K, V>(t : Trie<K, V>) : Nat
```

Get the size in O(1) time.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
var size = Trie.size(trie); // Returns 0, as `trie` is empty
assert(size == 0);
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
size := Trie.size(trie); // Returns 1, as we just added a new entry
assert(size == 1);
```

## Function `branch`
``` motoko no-repl
func branch<K, V>(l : Trie<K, V>, r : Trie<K, V>) : Trie<K, V>
```

Construct a branch node, computing the size stored there.

## Function `leaf`
``` motoko no-repl
func leaf<K, V>(kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V>
```

Construct a leaf node, computing the size stored there.

This helper function automatically enforces the MAX_LEAF_SIZE
by constructing branches as necessary; to do so, it also needs the bitpos
of the leaf.

## Function `fromList`
``` motoko no-repl
func fromList<K, V>(kvc : ?Nat, kvs : AssocList<Key<K>, V>, bitpos : Nat) : Trie<K, V>
```

Transform a list into a trie, splitting input list into small (leaf) lists, if necessary.

## Function `clone`
``` motoko no-repl
func clone<K, V>(t : Trie<K, V>) : Trie<K, V>
```

Clone the trie efficiently, via sharing.

Purely-functional representation permits _O(1)_ copy, via persistent sharing.

## Function `replace`
``` motoko no-repl
func replace<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : ?V) : (Trie<K, V>, ?V)
```

Replace the given key's value option with the given value, returning the modified trie.
Also returns the replaced value if the key existed and `null` otherwise.
Compares keys using the provided function `k_eq`.

Note: Replacing a key's value by `null` removes the key and also shrinks the trie.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "test", Text.equal, 1).0;
trie := Trie.replace(trie, key "test", Text.equal, 42).0;
assert (Trie.get(trie, key "hello", Text.equal) == ?42);
```

## Function `put`
``` motoko no-repl
func put<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : V) : (Trie<K, V>, ?V)
```

Put the given key's value in the trie; return the new trie, and the previous value associated with the key, if any.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
let previousValue = Trie.put(trie, key "hello", Text.equal, 33).1; // Returns ?42
assert(previousValue == ?42);
```

## Function `get`
``` motoko no-repl
func get<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V
```

Get the value of the given key in the trie, or return null if nonexistent.

For a more detailed overview of how to use a Trie,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
var value = Trie.get(trie, key "hello", Text.equal); // Returns `?42`
assert(value == ?42);
value := Trie.get(trie, key "world", Text.equal); // Returns `null`
assert(value == null);
```

## Function `find`
``` motoko no-repl
func find<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : ?V
```

Find the given key's value in the trie, or return `null` if nonexistent

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
var value = Trie.find(trie, key "hello", Text.equal); // Returns `?42`
assert(value == ?42);
value := Trie.find(trie, key "world", Text.equal); // Returns `null`
assert(value == null);
```

## Function `merge`
``` motoko no-repl
func merge<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V>
```

Merge tries, preferring the left trie where there are collisions
in common keys.

note: the `disj` operation generalizes this `merge`
operation in various ways, and does not (in general) lose
information; this operation is a simpler, special case.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 42).0;
// trie2 is a copy of trie
var trie2 = Trie.clone(trie);
// trie2 has a different value for "hello"
trie2 := Trie.put(trie2, key "hello", Text.equal, 33).0;
// mergedTrie has the value 42 for "hello", as the left trie is preferred
// in the case of a collision
var mergedTrie = Trie.merge(trie, trie2, Text.equal);
var value = Trie.get(mergedTrie, key "hello", Text.equal);
assert(value == ?42);
```

## Function `mergeDisjoint`
``` motoko no-repl
func mergeDisjoint<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, k_eq : (K, K) -> Bool) : Trie<K, V>
```

<a name="mergedisjoint"></a>

Merge tries like `merge`, but traps if there are collisions in common keys between the
left and right inputs.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 42).0;
// trie2 is a copy of trie
var trie2 = Trie.clone(trie);
// trie2 has a different value for "hello"
trie2 := Trie.put(trie2, key "hello", Text.equal, 33).0;
// `mergeDisjoint` signals a dynamic errror
// in the case of a collision
var mergedTrie = Trie.mergeDisjoint(trie, trie2, Text.equal);
```

## Function `diff`
``` motoko no-repl
func diff<K, V, W>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool) : Trie<K, V>
```

Difference of tries. The output consists of pairs of
the left trie whose keys are not present in the right trie; the
values of the right trie are irrelevant.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 42).0;
// trie2 is a copy of trie
var trie2 = Trie.clone(trie);
// trie2 now has an additional key
trie2 := Trie.put(trie2, key "ciao", Text.equal, 33).0;
// `diff` returns a trie with the key "ciao",
// as this key is not present in `trie`
// (note that we pass `trie2` as the left trie)
Trie.diff(trie2, trie, Text.equal);
```

## Function `disj`
``` motoko no-repl
func disj<K, V, W, X>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool, vbin : (?V, ?W) -> X) : Trie<K, X>
```

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
``` motoko no-repl
func join<K, V, W, X>(tl : Trie<K, V>, tr : Trie<K, W>, k_eq : (K, K) -> Bool, vbin : (V, W) -> X) : Trie<K, X>
```

Map join.

Implements the database idea of an ["inner join"](https://stackoverflow.com/questions/38549/what-is-the-difference-between-inner-join-and-outer-join).

This operation generalizes the notion of "set intersection" to
finite maps.  The values of matching keys are combined with the given binary
operator, and unmatched key-value pairs are not present in the output.


## Function `foldUp`
``` motoko no-repl
func foldUp<K, V, X>(t : Trie<K, V>, bin : (X, X) -> X, leaf : (K, V) -> X, empty : X) : X
```

This operation gives a recursor for the internal structure of
tries.  Many common operations are instantiations of this function,
either as clients, or as hand-specialized versions (e.g., see , map,
mapFilter, some and all below).

## Function `prod`
``` motoko no-repl
func prod<K1, V1, K2, V2, K3, V3>(tl : Trie<K1, V1>, tr : Trie<K2, V2>, op : (K1, V1, K2, V2) -> ?(Key<K3>, V3), k3_eq : (K3, K3) -> Bool) : Trie<K3, V3>
```

Map product.

Conditional _catesian product_, where the given
operation `op` _conditionally_ creates output elements in the
resulting trie.

The keyed structure of the input tries are not relevant for this
operation: all pairs are considered, regardless of keys matching or
not.  Moreover, the resulting trie may use keys that are unrelated to
these input keys.


## Function `iter`
``` motoko no-repl
func iter<K, V>(t : Trie<K, V>) : I.Iter<(K, V)>
```

Returns an iterator of type `Iter` over the key-value entries of the trie.

Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
// create an Iterator over key-value pairs of trie
let iter = Trie.iter(trie);
// add another key-value pair to `trie`.
// because we created our iterator before
// this update, it will not contain this new key-value pair
trie := Trie.put(trie, key "ciao", Text.equal, 3).0;
var sum : Nat = 0;
for ((k,v) in iter) {
  sum += v;
};
assert(sum == 74);
```

## Module `Build`

``` motoko no-repl
module Build
```

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


### Type `Build`
``` motoko no-repl
type Build<K, V> = {#skip; #put : (K, ?Hash.Hash, V); #seq : { size : Nat; left : Build<K, V>; right : Build<K, V> }}
```

The build of a trie, as an AST for a simple DSL.


### Function `size`
``` motoko no-repl
func size<K, V>(tb : Build<K, V>) : Nat
```

Size of the build, measured in `#put` operations


### Function `seq`
``` motoko no-repl
func seq<K, V>(l : Build<K, V>, r : Build<K, V>) : Build<K, V>
```

Build sequence of two sub-builds


### Function `prod`
``` motoko no-repl
func prod<K1, V1, K2, V2, K3, V3>(tl : Trie<K1, V1>, tr : Trie<K2, V2>, op : (K1, V1, K2, V2) -> ?(K3, V3), _k3_eq : (K3, K3) -> Bool) : Build<K3, V3>
```

Like [`prod`](#prod), except do not actually do the put calls, just
record them, as a (binary tree) data structure, isomorphic to the
recursion of this function (which is balanced, in expectation).


### Function `nth`
``` motoko no-repl
func nth<K, V>(tb : Build<K, V>, i : Nat) : ?(K, ?Hash.Hash, V)
```

Project the nth key-value pair from the trie build.

This position is meaningful only when the build contains multiple uses of one or more keys, otherwise it is not.


### Function `projectInner`
``` motoko no-repl
func projectInner<K1, K2, V>(t : Trie<K1, Build<K2, V>>) : Build<K2, V>
```

Like [`mergeDisjoint`](#mergedisjoint), except that it avoids the
work of actually merging any tries; rather, just record the work for
latter (if ever).


### Function `toArray`
``` motoko no-repl
func toArray<K, V, W>(tb : Build<K, V>, f : (K, V) -> W) : [W]
```

Gather the collection of key-value pairs into an array of a (possibly-distinct) type.

## Function `fold`
``` motoko no-repl
func fold<K, V, X>(t : Trie<K, V>, f : (K, V, X) -> X, x : X) : X
```

Fold over the key-value pairs of the trie, using an accumulator.
The key-value pairs have no reliable or meaningful ordering.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 3).0;
// create an accumulator, in our case the sum of all values
func calculateSum(k : Text, v : Nat, acc : Nat) : Nat = acc + v;
// Fold over the trie using the accumulator.
// Note that 0 is the initial value of the accumulator.
let sum = Trie.fold(trie, calculateSum, 0);
assert(sum == 77);
```

## Function `some`
``` motoko no-repl
func some<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool
```

Test whether a given key-value pair is present, or not.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 3).0;
// `some` takes a function that returns a Boolean indicating whether
// the key-value pair is present or not
var isPresent = Trie.some(
  trie,
  func(k : Text, v : Nat) : Bool = k == "bye" and v == 32,
);
assert(isPresent == true);
isPresent := Trie.some(
  trie,
  func(k : Text, v : Nat) : Bool = k == "hello" and v == 32,
);
assert(isPresent == false);
```

## Function `all`
``` motoko no-repl
func all<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Bool
```

Test whether all key-value pairs have a given property.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `all` takes a function that returns a boolean indicating whether
// the key-value pairs all have a given property, in our case that
// all values are greater than 9
var hasProperty = Trie.all(
  trie,
  func(k : Text, v : Nat) : Bool = v > 9,
);
assert(hasProperty == true);
// now we check if all values are greater than 100
hasProperty := Trie.all(
  trie,
  func(k : Text, v : Nat) : Bool = v > 100,
);
assert(hasProperty == false);
```

## Function `nth`
``` motoko no-repl
func nth<K, V>(t : Trie<K, V>, i : Nat) : ?(Key<K>, V)
```

Project the nth key-value pair from the trie.

Note: This position is not meaningful; it's only here so that we
can inject tries into arrays using functions like `Array.tabulate`.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
import Array "mo:base/Array";
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `tabulate` takes a size parameter, so we check the size of
// the trie first
let size = Trie.size(trie);
// Now we can create an array of the same size passing `nth` as
// the generator used to fill the array.
// Note that `toArray` is a convenience function that does the
// same thing without you having to check whether the tuple is
// `null` or not, which we're not doing in this example
let array = Array.tabulate<?(Key<Text>, Nat)>(
  size,
  func n = Trie.nth(trie, n)
);
```

## Function `toArray`
``` motoko no-repl
func toArray<K, V, W>(t : Trie<K, V>, f : (K, V) -> W) : [W]
```

Gather the collection of key-value pairs into an array of a (possibly-distinct) type.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `toArray` takes a function that takes a key-value tuple
// and returns a value of the type you want to use to fill
// the array.
// In our case we just return the value
let array = Trie.toArray<Text, Nat, Nat>(
  trie,
  func (k, v) = v
);
```

## Function `isEmpty`
``` motoko no-repl
func isEmpty<K, V>(t : Trie<K, V>) : Bool
```

Test for "deep emptiness": subtrees that have branching structure,
but no leaves.  These can result from naive filtering operations;
filter uses this function to avoid creating such subtrees.

## Function `filter`
``` motoko no-repl
func filter<K, V>(t : Trie<K, V>, f : (K, V) -> Bool) : Trie<K, V>
```

Filter the key-value pairs by a given predicate.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `filter` takes a function that takes a key-value tuple
// and returns true if the key-value pair should be included.
// In our case those are pairs with a value greater than 20
let filteredTrie = Trie.filter<Text, Nat>(
  trie,
  func (k, v) = v > 20
);
assert (Trie.all<Text, Nat>(filteredTrie, func(k, v) = v > 20) == true);
```

## Function `mapFilter`
``` motoko no-repl
func mapFilter<K, V, W>(t : Trie<K, V>, f : (K, V) -> ?W) : Trie<K, W>
```

Map and filter the key-value pairs by a given predicate.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `mapFilter` takes a function that takes a key-value tuple
// and returns a possibly-distinct value if the key-value pair should be included.
// In our case, we filter for values greater than 20 and map them to their square.
let filteredTrie = Trie.mapFilter<Text, Nat, Nat>(
  trie,
  func (k, v) = if (v > 20) return ?(v**2) else return null
);
assert (Trie.all<Text, Nat>(filteredTrie, func(k, v) = v > 60) == true);
```

## Function `equalStructure`
``` motoko no-repl
func equalStructure<K, V>(tl : Trie<K, V>, tr : Trie<K, V>, keq : (K, K) -> Bool, veq : (V, V) -> Bool) : Bool
```

Test for equality, but naively, based on structure.
Does not attempt to remove "junk" in the tree;
For instance, a "smarter" approach would equate
  `#bin {left = #empty; right = #empty}`
with
  `#empty`.
We do not observe that equality here.

## Function `replaceThen`
``` motoko no-repl
func replaceThen<K, V, X>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v2 : V, success : (Trie<K, V>, V) -> X, fail : () -> X) : X
```

Replace the given key's value in the trie,
and only if successful, do the success continuation,
otherwise, return the failure value

For a more detailed overview of how to use a Trie,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
trie := Trie.put(trie, key "ciao", Text.equal, 10).0;
// `replaceThen` takes the same arguments as `replace` but also a success continuation
// and a failure connection that are called in the respective scenarios.
// if the replace fails, that is the key is not present in the trie, the failure continuation is called.
// if the replace succeeds, that is the key is present in the trie, the success continuation is called.
// in this example we are simply returning the Text values `success` and `fail` respectively.
var continuation = Trie.replaceThen<Text, Nat, Text>(
  trie,
  key "hello",
  Text.equal,
  12,
  func (t, v) = "success",
  func () = "fail"
);
assert (continuation == "success");
continuation := Trie.replaceThen<Text, Nat, Text>(
  trie,
  key "shalom",
  Text.equal,
  12,
  func (t, v) = "success",
  func () = "fail"
);
assert (continuation == "fail");
```

## Function `putFresh`
``` motoko no-repl
func putFresh<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, v : V) : Trie<K, V>
```

Put the given key's value in the trie; return the new trie; assert that no prior value is associated with the key

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
// note that compared to `put`, `putFresh` does not return a tuple
trie := Trie.putFresh(trie, key "hello", Text.equal, 42);
trie := Trie.putFresh(trie, key "bye", Text.equal, 32);
// this will fail as "hello" is already present in the trie
trie := Trie.putFresh(trie, key "hello", Text.equal, 10);
```

## Function `put2D`
``` motoko no-repl
func put2D<K1, K2, V>(t : Trie2D<K1, K2, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, v : V) : Trie2D<K1, K2, V>
```

Put the given key's value in the 2D trie; return the new 2D trie.

## Function `put3D`
``` motoko no-repl
func put3D<K1, K2, K3, V>(t : Trie3D<K1, K2, K3, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, k3 : Key<K3>, k3_eq : (K3, K3) -> Bool, v : V) : Trie3D<K1, K2, K3, V>
```

Put the given key's value in the trie; return the new trie;

## Function `remove`
``` motoko no-repl
func remove<K, V>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool) : (Trie<K, V>, ?V)
```

Remove the entry for the given key from the trie, by returning the reduced trie.
Also returns the removed value if the key existed and `null` otherwise.
Compares keys using the provided function `k_eq`.

Note: The removal of an existing key shrinks the trie.

For a more detailed overview of how to use a `Trie`,
see the [User's Overview](#overview).

Example:
```motoko include=initialize
trie := Trie.put(trie, key "hello", Text.equal, 42).0;
trie := Trie.put(trie, key "bye", Text.equal, 32).0;
// remove the entry associated with "hello"
trie := Trie.remove(trie, key "hello", Text.equal).0;
assert (Trie.get(trie, key "hello", Text.equal) == null);
```

## Function `removeThen`
``` motoko no-repl
func removeThen<K, V, X>(t : Trie<K, V>, k : Key<K>, k_eq : (K, K) -> Bool, success : (Trie<K, V>, V) -> X, fail : () -> X) : X
```

Remove the given key's value in the trie,
and only if successful, do the success continuation,
otherwise, return the failure value

## Function `remove2D`
``` motoko no-repl
func remove2D<K1, K2, V>(t : Trie2D<K1, K2, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool) : (Trie2D<K1, K2, V>, ?V)
```

remove the given key-key pair's value in the 2D trie; return the
new trie, and the prior value, if any.

## Function `remove3D`
``` motoko no-repl
func remove3D<K1, K2, K3, V>(t : Trie3D<K1, K2, K3, V>, k1 : Key<K1>, k1_eq : (K1, K1) -> Bool, k2 : Key<K2>, k2_eq : (K2, K2) -> Bool, k3 : Key<K3>, k3_eq : (K3, K3) -> Bool) : (Trie3D<K1, K2, K3, V>, ?V)
```

Remove the given key-key pair's value in the 3D trie; return the
new trie, and the prior value, if any.

## Function `mergeDisjoint2D`
``` motoko no-repl
func mergeDisjoint2D<K1, K2, V>(t : Trie2D<K1, K2, V>, _k1_eq : (K1, K1) -> Bool, k2_eq : (K2, K2) -> Bool) : Trie<K2, V>
```

Like [`mergeDisjoint`](#mergedisjoint), except instead of merging a
pair, it merges the collection of dimension-2 sub-trees of a 2D
trie.
