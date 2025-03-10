# RBTree
Key-value map implemented as a red-black tree (RBTree) with nodes storing key-value pairs.

A red-black tree is a balanced binary search tree ordered by the keys.

The tree data structure internally colors each of its nodes either red or black,
and uses this information to balance the tree during the modifying operations.

Creation:
Instantiate class `RBTree<K, V>` that provides a map from keys of type `K` to values of type `V`.

Example:
```motoko
import RBTree "mo:base/RBTree";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let tree = RBTree.RBTree<Nat, Text>(Nat.compare); // Create a new red-black tree mapping Nat to Text
tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "tree");
for (entry in tree.entries()) {
  Debug.print("Entry key=" # debug_show(entry.0) # " value=\"" # entry.1 #"\"");
}
```

Performance:
* Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
* Heap space: `O(n)` for storing the entire tree.
* Stack space: `O(log(n)) for storing the entire tree.
`n` denotes the number of key-value entries (i.e. nodes) stored in the tree.

Note:
* Tree insertion, replacement, and removal produce `O(log(n))` garbage objects.

Credits:

The core of this implementation is derived from:

* Ken Friis Larsen's [RedBlackMap.sml](https://github.com/kfl/mosml/blob/master/src/mosmllib/Redblackmap.sml), which itself is based on:
* Stefan Kahrs, "Red-black trees with types", Journal of Functional Programming, 11(4): 425-432 (2001), [version 1 in web appendix](http://www.cs.ukc.ac.uk/people/staff/smk/redblack/rb.html).

## Type `Color`
``` motoko no-repl
type Color = {#R; #B}
```

Node color: Either red (`#R`) or black (`#B`).

## Type `Tree`
``` motoko no-repl
type Tree<K, V> = {#node : (Color, Tree<K, V>, (K, ?V), Tree<K, V>); #leaf}
```

Red-black tree of nodes with key-value entries, ordered by the keys.
The keys have the generic type `K` and the values the generic type `V`.
Leaves are considered implicitly black.

## Class `RBTree<K, V>`

``` motoko no-repl
class RBTree<K, V>(compare : (K, K) -> O.Order)
```

A map from keys of type `K` to values of type `V` implemented as a red-black tree.
The entries of key-value pairs are ordered by `compare` function applied to the keys.

The class enables imperative usage in object-oriented-style.
However, internally, the class uses a functional implementation.

The `compare` function should implement a consistent total order among all possible values of `K` and
for efficiency, only involves `O(1)` runtime costs without space allocation.

Example:
```motoko name=initialize
import RBTree "mo:base/RBTree";
import Nat "mo:base/Nat";

let tree = RBTree.RBTree<Nat, Text>(Nat.compare); // Create a map of `Nat` to `Text` using the `Nat.compare` order
```

Costs of instantiation (only empty tree):
Runtime: `O(1)`.
Heap space: `O(1)`.
Stack space: `O(1)`.

### Function `share`
``` motoko no-repl
func share() : Tree<K, V>
```

Return a snapshot of the internal functional tree representation as sharable data.
The returned tree representation is not affected by subsequent changes of the `RBTree` instance.


Example:
```motoko include=initialize

tree.put(1, "one");
let treeSnapshot = tree.share();
tree.put(2, "second");
RBTree.size(treeSnapshot) // => 1 (Only the first insertion is part of the snapshot.)
```

Useful for storing the state of a tree object as a stable variable, determining its size, pretty-printing, and sharing it across async function calls,
i.e. passing it in async arguments or async results.

Runtime: `O(1)`.
Heap space: `O(1)`.
Stack space: `O(1)`.


### Function `unshare`
``` motoko no-repl
func unshare(t : Tree<K, V>) : ()
```

Reset the current state of the tree object from a functional tree representation.

Example:
```motoko include=initialize
import Iter "mo:base/Iter";

tree.put(1, "one");
let snapshot = tree.share(); // save the current state of the tree object in a snapshot
tree.put(2, "two");
tree.unshare(snapshot); // restore the tree object from the snapshot
Iter.toArray(tree.entries()) // => [(1, "one")]
```

Useful for restoring the state of a tree object from stable data, saved, for example, in a stable variable.

Runtime: `O(1)`.
Heap space: `O(1)`.
Stack space: `O(1)`.


### Function `get`
``` motoko no-repl
func get(key : K) : ?V
```

Retrieve the value associated with a given key, if present. Returns `null`, if the key is absent.
The key is searched according to the `compare` function defined on the class instantiation.

Example:
```motoko include=initialize

tree.put(1, "one");
tree.put(2, "two");

tree.get(1) // => ?"one"
```

Runtime: `O(log(n))`.
Heap space: `O(1)`.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree and
assuming that the `compare` function implements an `O(1)` comparison.


### Function `replace`
``` motoko no-repl
func replace(key : K, value : V) : ?V
```

Replace the value associated with a given key, if the key is present.
Otherwise, if the key does not yet exist, insert the key-value entry.

Returns the previous value of the key, if the key already existed.
Otherwise, `null`, if the key did not yet exist before.

Example:
```motoko include=initialize
import Iter "mo:base/Iter";

tree.put(1, "old one");
tree.put(2, "two");

ignore tree.replace(1, "new one");
Iter.toArray(tree.entries()) // => [(1, "new one"), (2, "two")]
```

Runtime: `O(log(n))`.
Heap space: `O(1)` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` garbage objects.


### Function `put`
``` motoko no-repl
func put(key : K, value : V)
```

Insert a key-value entry in the tree. If the key already exists, it overwrites the associated value.

Example:
```motoko include=initialize
import Iter "mo:base/Iter";

tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "three");
Iter.toArray(tree.entries()) // now contains three entries
```

Runtime: `O(log(n))`.
Heap space: `O(1)` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` garbage objects.


### Function `delete`
``` motoko no-repl
func delete(key : K)
```

Delete the entry associated with a given key, if the key exists.
No effect if the key is absent. Same as `remove(key)` except that it
does not have a return value.

Example:
```motoko include=initialize
import Iter "mo:base/Iter";

tree.put(1, "one");
tree.put(2, "two");

tree.delete(1);
Iter.toArray(tree.entries()) // => [(2, "two")].
```

Runtime: `O(log(n))`.
Heap space: `O(1)` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` temporary objects that will be collected as garbage.


### Function `remove`
``` motoko no-repl
func remove(key : K) : ?V
```

Remove the entry associated with a given key, if the key exists, and return the associated value.
Returns `null` without any other effect if the key is absent.

Example:
```motoko include=initialize
import Iter "mo:base/Iter";

tree.put(1, "one");
tree.put(2, "two");

ignore tree.remove(1);
Iter.toArray(tree.entries()) // => [(2, "two")].
```

Runtime: `O(log(n))`.
Heap space: `O(1)` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree and
assuming that the `compare` function implements an `O(1)` comparison.

Note: Creates `O(log(n))` garbage objects.


### Function `entries`
``` motoko no-repl
func entries() : I.Iter<(K, V)>
```

An iterator for the key-value entries of the map, in ascending key order.
The iterator takes a snapshot view of the tree and is not affected by concurrent modifications.

Example:
```motoko include=initialize
import Debug "mo:base/Debug";

tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "two");

for (entry in tree.entries()) {
  Debug.print("Entry key=" # debug_show(entry.0) # " value=\"" # entry.1 #"\"");
}

// Entry key=1 value="one"
// Entry key=2 value="two"
// Entry key=3 value="three"
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Heap space: `O(log(n))` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree.

Note: Full tree iteration creates `O(n)` temporary objects that will be collected as garbage.


### Function `entriesRev`
``` motoko no-repl
func entriesRev() : I.Iter<(K, V)>
```

An iterator for the key-value entries of the map, in descending key order.
The iterator takes a snapshot view of the tree and is not affected by concurrent modifications.

Example:
```motoko include=initialize
import Debug "mo:base/Debug";

let tree = RBTree.RBTree<Nat, Text>(Nat.compare);
tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "two");

for (entry in tree.entriesRev()) {
  Debug.print("Entry key=" # debug_show(entry.0) # " value=\"" # entry.1 #"\"");
}

// Entry key=3 value="three"
// Entry key=2 value="two"
// Entry key=1 value="one"
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Heap space: `O(log(n))` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree.

Note: Full tree iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `iter`
``` motoko no-repl
func iter<X, Y>(tree : Tree<X, Y>, direction : {#fwd; #bwd}) : I.Iter<(X, Y)>
```

Get an iterator for the entries of the `tree`, in ascending (`#fwd`) or descending (`#bwd`) order as specified by `direction`.
The iterator takes a snapshot view of the tree and is not affected by concurrent modifications.

Example:
```motoko
import RBTree "mo:base/RBTree";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

let tree = RBTree.RBTree<Nat, Text>(Nat.compare);
tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "two");

for (entry in RBTree.iter(tree.share(), #bwd)) { // backward iteration
  Debug.print("Entry key=" # debug_show(entry.0) # " value=\"" # entry.1 #"\"");
}

// Entry key=3 value="three"
// Entry key=2 value="two"
// Entry key=1 value="one"
```

Cost of iteration over all elements:
Runtime: `O(n)`.
Heap space: `O(log(n))` retained memory plus garbage, see the note below.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree.

Note: Full tree iteration creates `O(n)` temporary objects that will be collected as garbage.

## Function `size`
``` motoko no-repl
func size<X, Y>(t : Tree<X, Y>) : Nat
```

Determine the size of the tree as the number of key-value entries.

Example:
```motoko
import RBTree "mo:base/RBTree";
import Nat "mo:base/Nat";

let tree = RBTree.RBTree<Nat, Text>(Nat.compare);
tree.put(1, "one");
tree.put(2, "two");
tree.put(3, "three");

RBTree.size(tree.share()) // 3 entries
```

Runtime: `O(log(n))`.
Heap space: `O(1)`.
Stack space: `O(log(n))`.
where `n` denotes the number of key-value entries stored in the tree.
