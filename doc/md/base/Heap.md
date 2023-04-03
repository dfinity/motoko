# Heap
Class `Heap<X>` provides a priority queue of elements of type `X`.

The class wraps a purely-functional implementation based on a leftist heap.

Note on the constructor:
The constructor takes in a comparison function `compare` that defines the
ordering between elements of type `X`. Most primitive types have a default
version of this comparison function defined in their modules (e.g. `Nat.compare`).
The runtime analysis in this documentation assumes that the `compare` function
runs in `O(1)` time and space.

Example:
```motoko name=initialize
import Heap "mo:base/Heap";
import Text "mo:base/Text";

let heap = Heap.Heap<Text>(Text.compare);
```

Runtime: `O(1)`

Space: `O(1)`

## Type `Tree`
``` motoko no-repl
type Tree<X> = ?(Int, X, Tree<X>, Tree<X>)
```


## Class `Heap<X>`

``` motoko no-repl
class Heap<X>(compare : (X, X) -> O.Order)
```


### Function `put`
``` motoko no-repl
func put(x : X)
```

Inserts an element into the heap.

Example:
```motoko include=initialize

heap.put("apple");
heap.peekMin() // => ?"apple"
```

Runtime: `O(log(n))`

Space: `O(log(n))`


### Function `peekMin`
``` motoko no-repl
func peekMin() : ?X
```

Return the minimal element in the heap, or `null` if the heap is empty.

Example:
```motoko include=initialize

heap.put("apple");
heap.put("banana");
heap.put("cantaloupe");
heap.peekMin() // => ?"apple"
```

Runtime: `O(1)`

Space: `O(1)`


### Function `deleteMin`
``` motoko no-repl
func deleteMin()
```

Delete the minimal element in the heap, if it exists.

Example:
```motoko include=initialize

heap.put("apple");
heap.put("banana");
heap.put("cantaloupe");
heap.deleteMin();
heap.peekMin(); // => ?"banana"
```

Runtime: `O(log(n))`

Space: `O(log(n))`


### Function `removeMin`
``` motoko no-repl
func removeMin() : (minElement : ?X)
```

Delete and return the minimal element in the heap, if it exists.

Example:
```motoko include=initialize

heap.put("apple");
heap.put("banana");
heap.put("cantaloupe");
heap.removeMin(); // => ?"apple"
```

Runtime: `O(log(n))`

Space: `O(log(n))`


### Function `share`
``` motoko no-repl
func share() : Tree<X>
```

Return a snapshot of the internal functional tree representation as sharable data.
The returned tree representation is not affected by subsequent changes of the `Heap` instance.

Example:
```motoko include=initialize

heap.put("banana");
heap.share();
```

Useful for storing the heap as a stable variable, pretty-printing, and sharing it across async function calls,
i.e. passing it in async arguments or async results.

Runtime: `O(1)`

Space: `O(1)`


### Function `unsafeUnshare`
``` motoko no-repl
func unsafeUnshare(tree : Tree<X>)
```

Rewraps a snapshot of a heap (obtained by `share()`) in a `Heap` instance.
The wrapping instance must be initialized with the same `compare`
function that created the snapshot.

Example:
```motoko include=initialize

heap.put("apple");
heap.put("banana");
let snapshot = heap.share();
let heapCopy = Heap.Heap<Text>(Text.compare);
heapCopy.unsafeUnshare(snapshot);
heapCopy.peekMin() // => ?"apple"
```

Useful for loading a stored heap from a stable variable or accesing a heap
snapshot passed from an async function call.

Runtime: `O(1)`.

Space: `O(1)`.

## Function `fromIter`
``` motoko no-repl
func fromIter<X>(iter : I.Iter<X>, compare : (X, X) -> O.Order) : Heap<X>
```

Returns a new `Heap`, containing all entries given by the iterator `iter`.
The new map is initialized with the provided `compare` function.

Example:
```motoko include=initialize
let entries = ["banana", "apple", "cantaloupe"];
let iter = entries.vals();

let newHeap = Heap.fromIter<Text>(iter, Text.compare);
newHeap.peekMin() // => ?"apple"
```

Runtime: `O(size)`

Space: `O(size)`
