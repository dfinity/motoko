# Buffer
Generic, extensible buffers

Generic, mutable sequences that grow to accommodate arbitrary numbers of elements.

Class `Buffer<X>` provides extensible, mutable sequences of elements of type `X`.
that can be efficiently produced and consumed with imperative code.
A buffer object can be extended by a single element or the contents of another buffer object.

When required, the current state of a buffer object can be converted to a fixed-size array of its elements.

Buffers complement Motoko's non-extensible array types
(arrays do not support efficient extension, because the size of an array is
determined at construction and cannot be changed).

## `class Buffer<X>`


### Function `add`
``` motoko no-repl
func add(elem : X)
```

Adds a single element to the buffer.


### Function `removeLast`
``` motoko no-repl
func removeLast() : ?X
```

Removes the item that was inserted last and returns it or `null` if no
elements had been added to the Buffer.


### Function `append`
``` motoko no-repl
func append(b : Buffer<X>)
```

Adds all elements in buffer `b` to this buffer.


### Function `size`
``` motoko no-repl
func size() : Nat
```

Returns the current number of elements.


### Function `clear`
``` motoko no-repl
func clear()
```

Resets the buffer.


### Function `clone`
``` motoko no-repl
func clone() : Buffer<X>
```

Returns a copy of this buffer.


### Function `vals`
``` motoko no-repl
func vals() : { next : () -> ?X }
```

Returns an `Iter` over the elements of this buffer.


### Function `toArray`
``` motoko no-repl
func toArray() : [X]
```

Creates a new array containing this buffer's elements.


### Function `toVarArray`
``` motoko no-repl
func toVarArray() : [var X]
```

Creates a mutable array containing this buffer's elements.


### Function `get`
``` motoko no-repl
func get(i : Nat) : X
```

Gets the `i`-th element of this buffer. Traps if  `i >= count`. Indexing is zero-based.


### Function `getOpt`
``` motoko no-repl
func getOpt(i : Nat) : ?X
```

Gets the `i`-th element of the buffer as an option. Returns `null` when `i >= count`. Indexing is zero-based.


### Function `put`
``` motoko no-repl
func put(i : Nat, elem : X)
```

Overwrites the current value of the `i`-entry of  this buffer with `elem`. Traps if the
index is out of bounds. Indexing is zero-based.
Create a stateful buffer class encapsulating a mutable array.

The argument `initCapacity` determines its initial capacity.
The underlying mutable array grows by doubling when its current
capacity is exceeded.

## Function `fromArray`
``` motoko no-repl
func fromArray<X>(elems : [X]) : Buffer<X>
```

Creates a buffer from immutable array elements.

## Function `fromVarArray`
``` motoko no-repl
func fromVarArray<X>(elems : [var X]) : Buffer<X>
```

Creates a buffer from the elements of a mutable array.
