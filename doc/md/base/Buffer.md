# Buffer
Class `Buffer<X>` provides a mutable list of elements of type `X`.
The class wraps and resizes an underyling array that holds the elements,
and thus is comparable to ArrayLists or Vectors in other languages.

When required, the current state of a buffer object can be converted to a fixed-size array of its elements.
This is recommended for example when storing a buffer to a stable variable.

Throughout this documentation, two terms come up that can be confused: `size`
and `capacity`. `size` is the length of the list that the buffer represents.
`capacity` is the length of the underyling array that backs this list.
`capacity` >= `size` is an invariant for this class.

Like arrays, elements in the buffer are ordered by indices from 0 to `size`-1.

WARNING: Certain operations are amortized O(1) time, such as `add`, but run
in worst case O(n) time. These worst case runtimes may exceed the cycles limit
per message if the size of the buffer is large enough. Grow these structures
with discretion. All amortized operations below also list the worst case runtime.

Constructor:
The argument `initCapacity` determines the initial capacity of the array.
The underlying array grows by a factor of 1.5 when its current capacity is
exceeded. Further, when the size of the buffer shrinks to be less than 1/4th
of the capacity, the underyling array is shrunk by a factor of 2.

Example:
```motoko name=initialize
import Buffer "mo:base/Buffer";

let buffer = Buffer.Buffer<Nat>(3); // Creates a new Buffer
```

Runtime: O(initCapacity)

Space: O(initCapacity)

## Class `Buffer<X>`

``` motoko no-repl
class Buffer<X>(initCapacity : Nat)
```


### Function `size`
``` motoko no-repl
func size() : Nat
```

Returns the current number of elements in the buffer.

Example:
```motoko include=initialize
buffer.size() // => 0
```

Runtime: O(1)

Space: O(1)


### Function `add`
``` motoko no-repl
func add(element : X)
```

Adds a single element to the end of the buffer, doubling
the size of the array if capacity is exceeded.

Example:
```motoko include=initialize

buffer.add(0); // add 0 to buffer
buffer.add(1);
buffer.add(2);
buffer.add(3); // causes underlying array to increase in capacity
Buffer.toArray(buffer) // => [0, 1, 2, 3]
```

Amortized Runtime: O(1), Worst Case Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size)


### Function `get`
``` motoko no-repl
func get(index : Nat) : X
```

Returns the element at index `index`. Traps if  `index >= size`. Indexing is zero-based.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.get(0); // => 10
```

Runtime: O(1)

Space: O(1)


### Function `getOpt`
``` motoko no-repl
func getOpt(index : Nat) : ?X
```

Returns the element at index `index` as an option.
Returns `null` when `index >= size`. Indexing is zero-based.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
let x = buffer.getOpt(0); // => ?10
let y = buffer.getOpt(2); // => null
```

Runtime: O(1)

Space: O(1)


### Function `put`
``` motoko no-repl
func put(index : Nat, element : X)
```

Overwrites the current element at `index` with `element`. Traps if
`index` >= size. Indexing is zero-based.

Example:
```motoko include=initialize

buffer.add(10);
buffer.put(0, 20); // overwrites 10 at index 0 with 20
Buffer.toArray(buffer) // => [20]
```

Runtime: O(1)

Space: O(1)


### Function `removeLast`
``` motoko no-repl
func removeLast() : ?X
```

Removes and returns the last item in the buffer or `null` if
the buffer is empty.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.removeLast(); // => ?11
```

Amortized Runtime: O(1), Worst Case Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size)


### Function `remove`
``` motoko no-repl
func remove(index : Nat) : X
```

Removes and returns the element at `index` from the buffer.
All elements with index > `index` are shifted one position to the left.
This may cause a downsizing of the array.

Traps if index >= size.

WARNING: Repeated removal of elements using this method is ineffecient
and might be a sign that you should consider a different data-structure
for your use case.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.add(12);
let x = buffer.remove(1); // evaluates to 11. 11 no longer in list.
Buffer.toArray(buffer) // => [10, 12]
```

Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size)


### Function `clear`
``` motoko no-repl
func clear()
```

Resets the buffer. Capacity is set to 8.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.add(12);
buffer.clear(); // buffer is now empty
Buffer.toArray(buffer) // => []
```

Runtime: O(1)

Space: O(1)


### Function `filterEntries`
``` motoko no-repl
func filterEntries(predicate : (Nat, X) -> Bool)
```

Removes all elements from the buffer for which the predicate returns false.
The predicate is given both the index of the element and the element itself.
This may cause a downsizing of the array.

Example:
```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.add(12);
buffer.filterEntries(func(_, x) = x % 2 == 0); // only keep even elements
Buffer.toArray(buffer) // => [10, 12]
```

Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size)


### Function `capacity`
``` motoko no-repl
func capacity() : Nat
```

Returns the capacity of the buffer (the length of the underlying array).

Example:
```motoko include=initialize

let buffer = Buffer.Buffer<Nat>(2); // underlying array has capacity 2
buffer.add(10);
let c1 = buffer.capacity(); // => 2
buffer.add(11);
buffer.add(12); // causes capacity to increase by factor of 1.5
let c2 = buffer.capacity(); // => 3
```

Runtime: O(1)

Space: O(1)


### Function `reserve`
``` motoko no-repl
func reserve(capacity : Nat)
```

Changes the capacity to `capacity`. Traps if `capacity` < `size`.

```motoko include=initialize

buffer.reserve(4);
buffer.add(10);
buffer.add(11);
buffer.capacity(); // => 4
```

Runtime: O(capacity)

Space: O(capacity)


### Function `append`
``` motoko no-repl
func append(buffer2 : Buffer<X>)
```

Adds all elements in buffer `b` to this buffer.

```motoko include=initialize
let buffer1 = Buffer.Buffer<Nat>(2);
let buffer2 = Buffer.Buffer<Nat>(2);
buffer1.add(10);
buffer1.add(11);
buffer2.add(12);
buffer2.add(13);
buffer1.append(buffer2); // adds elements from buffer2 to buffer1
Buffer.toArray(buffer1) // => [10, 11, 12, 13]
```

Amortized Runtime: O(size2), Worst Case Runtime: O(size1 + size2)

Amortized Space: O(1), Worst Case Space: O(size1 + size2)


### Function `insert`
``` motoko no-repl
func insert(index : Nat, element : X)
```

Inserts `element` at `index`, shifts all elements to the right of
`index` over by one index. Traps if `index` is greater than size.

```motoko include=initialize
let buffer1 = Buffer.Buffer<Nat>(2);
let buffer2 = Buffer.Buffer<Nat>(2);
buffer.add(10);
buffer.add(11);
buffer.insert(1, 9);
Buffer.toArray(buffer) // => [10, 9, 11]
```

Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size)


### Function `insertBuffer`
``` motoko no-repl
func insertBuffer(index : Nat, buffer2 : Buffer<X>)
```

Inserts `buffer2` at `index`, and shifts all elements to the right of
`index` over by size2. Traps if `index` is greater than size.

```motoko include=initialize
let buffer1 = Buffer.Buffer<Nat>(2);
let buffer2 = Buffer.Buffer<Nat>(2);
buffer1.add(10);
buffer1.add(11);
buffer2.add(12);
buffer2.add(13);
buffer1.insertBuffer(1, buffer2);
Buffer.toArray(buffer1) // => [10, 12, 13, 11]
```

Runtime: O(size)

Amortized Space: O(1), Worst Case Space: O(size1 + size2)


### Function `sort`
``` motoko no-repl
func sort(compare : (X, X) -> Order.Order)
```

Sorts the elements in the buffer according to `compare`.
Sort is deterministic, stable, and in-place.

```motoko include=initialize

import Nat "mo:base/Nat";

buffer.add(11);
buffer.add(12);
buffer.add(10);
buffer.sort(Nat.compare);
Buffer.toArray(buffer) // => [10, 11, 12]
```

Runtime: O(size * log(size))

Space: O(size)


### Function `vals`
``` motoko no-repl
func vals() : { next : () -> ?X }
```

Returns an Iterator (`Iter`) over the elements of this buffer.
Iterator provides a single method `next()`, which returns
elements in order, or `null` when out of elements to iterate over.

```motoko include=initialize

buffer.add(10);
buffer.add(11);
buffer.add(12);

var sum = 0;
for (element in buffer.vals()) {
  sum += element;
};
sum // => 33
```

Runtime: O(1)

Space: O(1)


### Function `clone`
``` motoko no-repl
func clone() : Buffer<X>
```

@deprecated Use static library function instead.


### Function `toArray`
``` motoko no-repl
func toArray() : [X]
```

@deprecated Use static library function instead.


### Function `toVarArray`
``` motoko no-repl
func toVarArray() : [var X]
```

@deprecated Use static library function instead.

## Function `isEmpty`
``` motoko no-repl
func isEmpty<X>(buffer : Buffer<X>) : Bool
```

Returns true if and only if the buffer is empty.

Example:
```motoko include=initialize
buffer.add(2);
buffer.add(0);
buffer.add(3);
Buffer.isEmpty(buffer); // => false
```

```motoko include=initialize
Buffer.isEmpty(buffer); // => true
```

Runtime: O(1)

Space: O(1)

## Function `contains`
``` motoko no-repl
func contains<X>(buffer : Buffer<X>, element : X, equal : (X, X) -> Bool) : Bool
```

Returns true iff `buffer` contains `element` with respect to equality
defined by `equal`.


Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(2);
buffer.add(0);
buffer.add(3);
Buffer.contains<Nat>(buffer, 2, Nat.equal); // => true
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `clone`
``` motoko no-repl
func clone<X>(buffer : Buffer<X>) : Buffer<X>
```

Returns a copy of `buffer`, with the same capacity.


Example:
```motoko include=initialize

buffer.add(1);

let clone = Buffer.clone(buffer);
Buffer.toArray(clone); // => [1]
```

Runtime: O(size)

Space: O(size)

## Function `max`
``` motoko no-repl
func max<X>(buffer : Buffer<X>, compare : (X, X) -> Order) : ?X
```

Finds the greatest element in `buffer` defined by `compare`.
Returns `null` if `buffer` is empty.


Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);

Buffer.max(buffer, Nat.compare); // => ?2
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `min`
``` motoko no-repl
func min<X>(buffer : Buffer<X>, compare : (X, X) -> Order) : ?X
```

Finds the least element in `buffer` defined by `compare`.
Returns `null` if `buffer` is empty.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);

Buffer.min(buffer, Nat.compare); // => ?1
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `equal`
``` motoko no-repl
func equal<X>(buffer1 : Buffer<X>, buffer2 : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Defines equality for two buffers, using `equal` to recursively compare elements in the
buffers. Returns true iff the two buffers are of the same size, and `equal`
evaluates to true for every pair of elements in the two buffers of the same
index.


Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let buffer1 = Buffer.Buffer<Nat>(2);
buffer1.add(1);
buffer1.add(2);

let buffer2 = Buffer.Buffer<Nat>(5);
buffer2.add(1);
buffer2.add(2);

Buffer.equal(buffer1, buffer2, Nat.equal); // => true
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `compare`
``` motoko no-repl
func compare<X>(buffer1 : Buffer<X>, buffer2 : Buffer<X>, compare : (X, X) -> Order.Order) : Order.Order
```

Defines comparison for two buffers, using `compare` to recursively compare elements in the
buffers. Comparison is defined lexicographically.


Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let buffer1 = Buffer.Buffer<Nat>(2);
buffer1.add(1);
buffer1.add(2);

let buffer2 = Buffer.Buffer<Nat>(3);
buffer2.add(3);
buffer2.add(4);

Buffer.compare<Nat>(buffer1, buffer2, Nat.compare); // => #less
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `toText`
``` motoko no-repl
func toText<X>(buffer : Buffer<X>, toText : X -> Text) : Text
```

Creates a textual representation of `buffer`, using `toText` to recursively
convert the elements into Text.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

Buffer.toText(buffer, Nat.toText); // => "[1, 2, 3, 4]"
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `toText` runs in O(1) time and space.

## Function `hash`
``` motoko no-repl
func hash<X>(buffer : Buffer<X>, hash : X -> Nat32) : Nat32
```

Hashes `buffer` using `hash` to hash the underlying elements.
The deterministic hash function is a function of the elements in the Buffer, as well
as their ordering.

Example:
```motoko include=initialize
import Hash "mo:base/Hash";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(1000);

Buffer.hash<Nat>(buffer, Hash.hash); // => 2_872_640_342
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `hash` runs in O(1) time and space.

## Function `indexOf`
``` motoko no-repl
func indexOf<X>(element : X, buffer : Buffer<X>, equal : (X, X) -> Bool) : ?Nat
```

Finds the first index of `element` in `buffer` using equality of elements defined
by `equal`. Returns `null` if `element` is not found.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

Buffer.indexOf<Nat>(3, buffer, Nat.equal); // => ?2
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `lastIndexOf`
``` motoko no-repl
func lastIndexOf<X>(element : X, buffer : Buffer<X>, equal : (X, X) -> Bool) : ?Nat
```

Finds the last index of `element` in `buffer` using equality of elements defined
by `equal`. Returns `null` if `element` is not found.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(2);
buffer.add(2);

Buffer.lastIndexOf<Nat>(2, buffer, Nat.equal); // => ?5
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `indexOfBuffer`
``` motoko no-repl
func indexOfBuffer<X>(subBuffer : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : ?Nat
```

Searches for `subBuffer` in `buffer`, and returns the starting index if it is found.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let sub = Buffer.Buffer<Nat>(2);
sub.add(4);
sub.add(5);
sub.add(6);

Buffer.indexOfBuffer<Nat>(sub, buffer, Nat.equal); // => ?3
```

Runtime: O(size of buffer + size of subBuffer)

Space: O(size of subBuffer)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `binarySearch`
``` motoko no-repl
func binarySearch<X>(element : X, buffer : Buffer<X>, compare : (X, X) -> Order.Order) : ?Nat
```

Similar to indexOf, but runs in logarithmic time. Assumes that `buffer` is sorted.
Behavior is undefined if `buffer` is not sorted. Uses `compare` to
perform the search. Returns an index of `element` if it is found.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(4);
buffer.add(5);
buffer.add(6);

Buffer.binarySearch<Nat>(5, buffer, Nat.compare); // => ?2
```

Runtime: O(log(size))

Space: O(1)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `subBuffer`
``` motoko no-repl
func subBuffer<X>(buffer : Buffer<X>, start : Nat, length : Nat) : Buffer<X>
```

Returns the sub-buffer of `buffer` starting at index `start`
of length `length`. Traps if `start` is out of bounds, or `start + length`
is greater than the size of `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let sub = Buffer.subBuffer(buffer, 3, 2);
Buffer.toText(sub, Nat.toText); // => [4, 5]
```

Runtime: O(length)

Space: O(length)

## Function `isSubBufferOf`
``` motoko no-repl
func isSubBufferOf<X>(subBuffer : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `subBuffer` is a sub-Buffer of `buffer`. Uses `equal` to
compare elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let sub = Buffer.Buffer<Nat>(2);
sub.add(2);
sub.add(3);
Buffer.isSubBufferOf(sub, buffer, Nat.equal); // => true
```

Runtime: O(size of subBuffer + size of buffer)

Space: O(size of subBuffer)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `isStrictSubBufferOf`
``` motoko no-repl
func isStrictSubBufferOf<X>(subBuffer : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `subBuffer` is a strict subBuffer of `buffer`, i.e. `subBuffer` must be
strictly contained inside both the first and last indices of `buffer`.
Uses `equal` to compare elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let sub = Buffer.Buffer<Nat>(2);
sub.add(2);
sub.add(3);
Buffer.isStrictSubBufferOf(sub, buffer, Nat.equal); // => true
```

Runtime: O(size of subBuffer + size of buffer)

Space: O(size of subBuffer)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `prefix`
``` motoko no-repl
func prefix<X>(buffer : Buffer<X>, length : Nat) : Buffer<X>
```

Returns the prefix of `buffer` of length `length`. Traps if `length`
is greater than the size of `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let pre = Buffer.prefix(buffer, 3); // => [1, 2, 3]
Buffer.toText(pre, Nat.toText);
```

Runtime: O(length)

Space: O(length)

## Function `isPrefixOf`
``` motoko no-repl
func isPrefixOf<X>(prefix : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `prefix` is a prefix of `buffer`. Uses `equal` to
compare elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let pre = Buffer.Buffer<Nat>(2);
pre.add(1);
pre.add(2);
Buffer.isPrefixOf(pre, buffer, Nat.equal); // => true
```

Runtime: O(size of prefix)

Space: O(size of prefix)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `isStrictPrefixOf`
``` motoko no-repl
func isStrictPrefixOf<X>(prefix : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `prefix` is a strict prefix of `buffer`. Uses `equal` to
compare elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let pre = Buffer.Buffer<Nat>(3);
pre.add(1);
pre.add(2);
pre.add(3);
Buffer.isStrictPrefixOf(pre, buffer, Nat.equal); // => true
```

Runtime: O(size of prefix)

Space: O(size of prefix)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `suffix`
``` motoko no-repl
func suffix<X>(buffer : Buffer<X>, length : Nat) : Buffer<X>
```

Returns the suffix of `buffer` of length `length`.
Traps if `length`is greater than the size of `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let suf = Buffer.suffix(buffer, 3); // => [2, 3, 4]
Buffer.toText(suf, Nat.toText);
```

Runtime: O(length)

Space: O(length)

## Function `isSuffixOf`
``` motoko no-repl
func isSuffixOf<X>(suffix : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `suffix` is a suffix of `buffer`. Uses `equal` to compare
elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let suf = Buffer.Buffer<Nat>(3);
suf.add(2);
suf.add(3);
suf.add(4);
Buffer.isSuffixOf(suf, buffer, Nat.equal); // => true
```

Runtime: O(length of suffix)

Space: O(length of suffix)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `isStrictSuffixOf`
``` motoko no-repl
func isStrictSuffixOf<X>(suffix : Buffer<X>, buffer : Buffer<X>, equal : (X, X) -> Bool) : Bool
```

Checks if `suffix` is a strict suffix of `buffer`. Uses `equal` to compare
elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);

let suf = Buffer.Buffer<Nat>(3);
suf.add(2);
suf.add(3);
suf.add(4);
Buffer.isStrictSuffixOf(suf, buffer, Nat.equal); // => true
```

Runtime: O(length of suffix)

Space: O(length of suffix)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `forAll`
``` motoko no-repl
func forAll<X>(buffer : Buffer<X>, predicate : X -> Bool) : Bool
```

Returns true iff every element in `buffer` satisfies `predicate`.

Example:
```motoko include=initialize

buffer.add(2);
buffer.add(3);
buffer.add(4);

Buffer.forAll<Nat>(buffer, func x { x > 1 }); // => true
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `forSome`
``` motoko no-repl
func forSome<X>(buffer : Buffer<X>, predicate : X -> Bool) : Bool
```

Returns true iff some element in `buffer` satisfies `predicate`.

Example:
```motoko include=initialize

buffer.add(2);
buffer.add(3);
buffer.add(4);

Buffer.forSome<Nat>(buffer, func x { x > 3 }); // => true
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `forNone`
``` motoko no-repl
func forNone<X>(buffer : Buffer<X>, predicate : X -> Bool) : Bool
```

Returns true iff no element in `buffer` satisfies `predicate`.

Example:
```motoko include=initialize

buffer.add(2);
buffer.add(3);
buffer.add(4);

Buffer.forNone<Nat>(buffer, func x { x == 0 }); // => true
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `toArray`
``` motoko no-repl
func toArray<X>(buffer : Buffer<X>) : [X]
```

Creates an array containing elements from `buffer`.

Example:
```motoko include=initialize

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.toArray<Nat>(buffer); // => [1, 2, 3]

```

Runtime: O(size)

Space: O(size)

## Function `toVarArray`
``` motoko no-repl
func toVarArray<X>(buffer : Buffer<X>) : [var X]
```

Creates a mutable array containing elements from `buffer`.

Example:
```motoko include=initialize

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.toVarArray<Nat>(buffer); // => [1, 2, 3]
```

Runtime: O(size)

Space: O(size)

## Function `fromArray`
``` motoko no-repl
func fromArray<X>(array : [X]) : Buffer<X>
```

Creates a buffer containing elements from `array`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let array = [2, 3];

let buf = Buffer.fromArray<Nat>(array); // => [2, 3]
Buffer.toText(buf, Nat.toText);
```

Runtime: O(size)

Space: O(size)

## Function `fromVarArray`
``` motoko no-repl
func fromVarArray<X>(array : [var X]) : Buffer<X>
```

Creates a buffer containing elements from `array`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let array = [var 1, 2, 3];

let buf = Buffer.fromVarArray<Nat>(array); // => [1, 2, 3]
Buffer.toText(buf, Nat.toText);
```

Runtime: O(size)

Space: O(size)

## Function `fromIter`
``` motoko no-repl
func fromIter<X>(iter : { next : () -> ?X }) : Buffer<X>
```

Creates a buffer containing elements from `iter`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let array = [1, 1, 1];
let iter = array.vals();

let buf = Buffer.fromIter<Nat>(iter); // => [1, 1, 1]
Buffer.toText(buf, Nat.toText);
```

Runtime: O(size)

Space: O(size)

## Function `trimToSize`
``` motoko no-repl
func trimToSize<X>(buffer : Buffer<X>)
```

Reallocates the array underlying `buffer` such that capacity == size.

Example:
```motoko include=initialize

let buffer = Buffer.Buffer<Nat>(10);
buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.trimToSize<Nat>(buffer);
buffer.capacity(); // => 3
```

Runtime: O(size)

Space: O(size)

## Function `map`
``` motoko no-repl
func map<X, Y>(buffer : Buffer<X>, f : X -> Y) : Buffer<Y>
```

Creates a new buffer by applying `f` to each element in `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let newBuf = Buffer.map<Nat, Nat>(buffer, func (x) { x + 1 });
Buffer.toText(newBuf, Nat.toText); // => [2, 3, 4]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `iterate`
``` motoko no-repl
func iterate<X>(buffer : Buffer<X>, f : X -> ())
```

Applies `f` to each element in `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.iterate<Nat>(buffer, func (x) {
  Debug.print(Nat.toText(x)); // prints each element in buffer
});
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapEntries`
``` motoko no-repl
func mapEntries<X, Y>(buffer : Buffer<X>, f : (Nat, X) -> Y) : Buffer<Y>
```

Applies `f` to each element in `buffer` and its index.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let newBuf = Buffer.mapEntries<Nat, Nat>(buffer, func (x, i) { x + i + 1 });
Buffer.toText(newBuf, Nat.toText); // => [2, 4, 6]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<X, Y>(buffer : Buffer<X>, f : X -> ?Y) : Buffer<Y>
```

Creates a new buffer by applying `f` to each element in `buffer`,
and keeping all non-null elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let newBuf = Buffer.mapFilter<Nat, Nat>(buffer, func (x) {
  if (x > 1) {
    ?(x * 2);
  } else {
    null;
  }
});
Buffer.toText(newBuf, Nat.toText); // => [4, 6]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapResult`
``` motoko no-repl
func mapResult<X, Y, E>(buffer : Buffer<X>, f : X -> Result.Result<Y, E>) : Result.Result<Buffer<Y>, E>
```

Creates a new buffer by applying `f` to each element in `buffer`.
If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
Returns an `#ok` containing the new buffer.

Example:
```motoko include=initialize
import Result "mo:base/Result";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let result = Buffer.mapResult<Nat, Nat, Text>(buffer, func (k) {
  if (k > 0) {
    #ok(k);
  } else {
    #err("One or more elements are zero.");
  }
});

Result.mapOk<Buffer.Buffer<Nat>, [Nat], Text>(result, func buffer = Buffer.toArray(buffer)) // => #ok([1, 2, 3])
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `chain`
``` motoko no-repl
func chain<X, Y>(buffer : Buffer<X>, k : X -> Buffer<Y>) : Buffer<Y>
```

Creates a new buffer by applying `k` to each element in `buffer`,
and concatenating the resulting buffers in order. This operation
is similar to what in other functional languages is known as monadic bind.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let chain = Buffer.chain<Nat, Nat>(buffer, func (x) {
  let b = Buffer.Buffer<Nat>(2);
  b.add(x);
  b.add(x * 2);
  return b;
});
Buffer.toText(chain, Nat.toText); // => [1, 2, 2, 4, 3, 6]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `k` runs in O(1) time and space.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<A, X>(buffer : Buffer<X>, base : A, combine : (A, X) -> A) : A
```

Collapses the elements in `buffer` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.foldLeft<Text, Nat>(buffer, "", func (acc, x) { acc # Nat.toText(x)}); // => "123"
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `combine` runs in O(1) time and space.

## Function `foldRight`
``` motoko no-repl
func foldRight<X, A>(buffer : Buffer<X>, base : A, combine : (X, A) -> A) : A
```

Collapses the elements in `buffer` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.foldRight<Nat, Text>(buffer, "", func (x, acc) { Nat.toText(x) # acc }); // => "123"
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `combine` runs in O(1) time and space.

## Function `first`
``` motoko no-repl
func first<X>(buffer : Buffer<X>) : X
```

Returns the first element of `buffer`. Traps if `buffer` is empty.

Example:
```motoko include=initialize

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.first(buffer); // => 1
```

Runtime: O(1)

Space: O(1)

## Function `last`
``` motoko no-repl
func last<X>(buffer : Buffer<X>) : X
```

Returns the last element of `buffer`. Traps if `buffer` is empty.

Example:
```motoko include=initialize

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.last(buffer); // => 3
```

Runtime: O(1)

Space: O(1)

## Function `make`
``` motoko no-repl
func make<X>(element : X) : Buffer<X>
```

Returns a new buffer with capacity and size 1, containing `element`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let buffer = Buffer.make<Nat>(1);
Buffer.toText(buffer, Nat.toText); // => [1]
```

Runtime: O(1)

Space: O(1)

## Function `reverse`
``` motoko no-repl
func reverse<X>(buffer : Buffer<X>)
```

Reverses the order of elements in `buffer`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.reverse(buffer);
Buffer.toText(buffer, Nat.toText); // => [3, 2, 1]
```

Runtime: O(size)

Space: O(1)

## Function `merge`
``` motoko no-repl
func merge<X>(buffer1 : Buffer<X>, buffer2 : Buffer<X>, compare : (X, X) -> Order) : Buffer<X>
```

Merges two sorted buffers into a single sorted buffer, using `compare` to define
the ordering. The final ordering is stable. Behavior is undefined if either
`buffer1` or `buffer2` is not sorted.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let buffer1 = Buffer.Buffer<Nat>(2);
buffer1.add(1);
buffer1.add(2);
buffer1.add(4);

let buffer2 = Buffer.Buffer<Nat>(2);
buffer2.add(2);
buffer2.add(4);
buffer2.add(6);

let merged = Buffer.merge<Nat>(buffer1, buffer2, Nat.compare);
Buffer.toText(merged, Nat.toText); // => [1, 2, 2, 4, 4, 6]
```

Runtime: O(size1 + size2)

Space: O(size1 + size2)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `removeDuplicates`
``` motoko no-repl
func removeDuplicates<X>(buffer : Buffer<X>, compare : (X, X) -> Order)
```

Eliminates all duplicate elements in `buffer` as defined by `compare`.
Elimination is stable with respect to the original ordering of the elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(1);
buffer.add(2);
buffer.add(3);

Buffer.removeDuplicates<Nat>(buffer, Nat.compare);
Buffer.toText(buffer, Nat.toText); // => [1, 2, 3]
```

Runtime: O(size * log(size))

Space: O(size)

## Function `partition`
``` motoko no-repl
func partition<X>(buffer : Buffer<X>, predicate : X -> Bool) : (Buffer<X>, Buffer<X>)
```

Splits `buffer` into a pair of buffers where all elements in the left
buffer satisfy `predicate` and all elements in the right buffer do not.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let partitions = Buffer.partition<Nat>(buffer, func (x) { x % 2 == 0 });
(Buffer.toArray(partitions.0), Buffer.toArray(partitions.1)) // => ([2, 4, 6], [1, 3, 5])
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `split`
``` motoko no-repl
func split<X>(buffer : Buffer<X>, index : Nat) : (Buffer<X>, Buffer<X>)
```

Splits the buffer into two buffers at `index`, where the left buffer contains
all elements with indices less than `index`, and the right buffer contains all
elements with indices greater than or equal to `index`. Traps if `index` is out
of bounds.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let split = Buffer.split<Nat>(buffer, 3);
(Buffer.toArray(split.0), Buffer.toArray(split.1)) // => ([1, 2, 3], [4, 5, 6])
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `chunk`
``` motoko no-repl
func chunk<X>(buffer : Buffer<X>, size : Nat) : Buffer<Buffer<X>>
```

Breaks up `buffer` into buffers of size `size`. The last chunk may
have less than `size` elements if the number of elements is not divisible
by the chunk size.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);
buffer.add(4);
buffer.add(5);
buffer.add(6);

let chunks = Buffer.chunk<Nat>(buffer, 3);
Buffer.toText<Buffer.Buffer<Nat>>(chunks, func buf = Buffer.toText(buf, Nat.toText)); // => [[1, 2, 3], [4, 5, 6]]
```

Runtime: O(number of elements in buffer)

Space: O(number of elements in buffer)

## Function `groupBy`
``` motoko no-repl
func groupBy<X>(buffer : Buffer<X>, equal : (X, X) -> Bool) : Buffer<Buffer<X>>
```

Groups equal and adjacent elements in the list into sub lists.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(2);
buffer.add(4);
buffer.add(5);
buffer.add(5);

let grouped = Buffer.groupBy<Nat>(buffer, func (x, y) { x == y });
Buffer.toText<Buffer.Buffer<Nat>>(grouped, func buf = Buffer.toText(buf, Nat.toText)); // => [[1], [2, 2], [4], [5, 5]]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `flatten`
``` motoko no-repl
func flatten<X>(buffer : Buffer<Buffer<X>>) : Buffer<X>
```

Flattens the buffer of buffers into a single buffer.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

let buffer = Buffer.Buffer<Buffer.Buffer<Nat>>(1);

let inner1 = Buffer.Buffer<Nat>(2);
inner1.add(1);
inner1.add(2);

let inner2 = Buffer.Buffer<Nat>(2);
inner2.add(3);
inner2.add(4);

buffer.add(inner1);
buffer.add(inner2);
// buffer = [[1, 2], [3, 4]]

let flat = Buffer.flatten<Nat>(buffer);
Buffer.toText<Nat>(flat, Nat.toText); // => [1, 2, 3, 4]
```

Runtime: O(number of elements in buffer)

Space: O(number of elements in buffer)

## Function `zip`
``` motoko no-repl
func zip<X, Y>(buffer1 : Buffer<X>, buffer2 : Buffer<Y>) : Buffer<(X, Y)>
```

Combines the two buffers into a single buffer of pairs, pairing together
elements with the same index. If one buffer is longer than the other, the
remaining elements from the longer buffer are not included.

Example:
```motoko include=initialize

let buffer1 = Buffer.Buffer<Nat>(2);
buffer1.add(1);
buffer1.add(2);
buffer1.add(3);

let buffer2 = Buffer.Buffer<Nat>(2);
buffer2.add(4);
buffer2.add(5);

let zipped = Buffer.zip(buffer1, buffer2);
Buffer.toArray(zipped); // => [(1, 4), (2, 5)]
```

Runtime: O(min(size1, size2))

Space: O(min(size1, size2))

## Function `zipWith`
``` motoko no-repl
func zipWith<X, Y, Z>(buffer1 : Buffer<X>, buffer2 : Buffer<Y>, zip : (X, Y) -> Z) : Buffer<Z>
```

Combines the two buffers into a single buffer, pairing together
elements with the same index and combining them using `zip`. If
one buffer is longer than the other, the remaining elements from
the longer buffer are not included.

Example:
```motoko include=initialize

let buffer1 = Buffer.Buffer<Nat>(2);
buffer1.add(1);
buffer1.add(2);
buffer1.add(3);

let buffer2 = Buffer.Buffer<Nat>(2);
buffer2.add(4);
buffer2.add(5);
buffer2.add(6);

let zipped = Buffer.zipWith<Nat, Nat, Nat>(buffer1, buffer2, func (x, y) { x + y });
Buffer.toArray(zipped) // => [5, 7, 9]
```

Runtime: O(min(size1, size2))

Space: O(min(size1, size2))

*Runtime and space assumes that `zip` runs in O(1) time and space.

## Function `takeWhile`
``` motoko no-repl
func takeWhile<X>(buffer : Buffer<X>, predicate : X -> Bool) : Buffer<X>
```

Creates a new buffer taking elements in order from `buffer` until predicate
returns false.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let newBuf = Buffer.takeWhile<Nat>(buffer, func (x) { x < 3 });
Buffer.toText(newBuf, Nat.toText); // => [1, 2]
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `dropWhile`
``` motoko no-repl
func dropWhile<X>(buffer : Buffer<X>, predicate : X -> Bool) : Buffer<X>
```

Creates a new buffer excluding elements in order from `buffer` until predicate
returns false.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

buffer.add(1);
buffer.add(2);
buffer.add(3);

let newBuf = Buffer.dropWhile<Nat>(buffer, func x { x < 3 }); // => [3]
Buffer.toText(newBuf, Nat.toText);
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `predicate` runs in O(1) time and space.
