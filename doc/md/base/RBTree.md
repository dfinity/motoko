# RBTree
Red-Black Trees

## Type `Color`

``` motoko
type Color = {#R; #B}
```

Node color: red or black.

## Type `Tree`

``` motoko
type Tree<X, Y> = {#node : (Color, Tree<X, Y>, (X, ?Y), Tree<X, Y>); #leaf}
```

Ordered, (red-black) tree of entries.

## `
class RBTree<X, Y>`


### Function `share`

``` motoko
func share() : Tree<X, Y>
```

Tree as sharable data.

Get non-OO, purely-functional representation:
for drawing, pretty-printing and non-OO contexts
(e.g., async args and results):


### Function `get`

``` motoko
func get(x : X) : ?Y
```

Get the value associated with a given key.


### Function `replace`

``` motoko
func replace(x : X, y : Y) : ?Y
```

Replace the value associated with a given key.


### Function `put`

``` motoko
func put(x : X, y : Y)
```

Put an entry: A value associated with a given key.


### Function `delete`

``` motoko
func delete(x : X)
```

Delete the entry associated with a given key.


### Function `remove`

``` motoko
func remove(x : X) : ?Y
```

Remove the entry associated with a given key.


### Function `entries`

``` motoko
func entries() : I.Iter<(X, Y)>
```

An iterator for the key-value entries of the map, in ascending key order.

iterator is persistent, like the tree itself


### Function `entriesRev`

``` motoko
func entriesRev() : I.Iter<(X, Y)>
```

An iterator for the key-value entries of the map, in descending key order.

iterator is persistent, like the tree itself
Create an order map from an order function for its keys.

## Function `iter`

``` motoko
func iter<X, Y>(t : Tree<X, Y>, dir : {#fwd; #bwd}) : I.Iter<(X, Y)>
```

An iterator for the entries of the map, in ascending (`#fwd`) or descending (`#bwd`) order.

## Function `size`

``` motoko
func size<X, Y>(t : Tree<X, Y>) : Nat
```

The size of the tree as the number of key-value entries.
