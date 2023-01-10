# Array
Provides extended utility functions on Arrays.

Note the difference between mutable and non-mutable arrays below.

WARNING: If you are looking for a list that can grow and shrink in size,
it is recommended you use either the Buffer class or the List class for
those purposes. Arrays must be created with a fixed size.

Import from the base library to use this module.
```motoko name=import
import Array "mo:base/Array";
```

## Function `init`
``` motoko no-repl
func init<X>(size : Nat, initValue : X) : [var X]
```

Create a mutable array with `size` copies of the initial value.

```motoko include=import
let array = Array.init<Nat>(4, 2);
```

Runtime: O(size)
Space: O(size)


## Function `tabulate`
``` motoko no-repl
func tabulate<X>(size : Nat, generator : Nat -> X) : [X]
```

Create an immutable array of size `size`. Each element at index i
is created by applying `generator` to i.

```motoko include=import
let array : [Nat] = Array.tabulate<Nat>(4, func i = i * 2);
```

Runtime: O(size)
Space: O(size)

*Runtime and space assumes that `generator` runs in O(1) time and space.


## Function `tabulateVar`
``` motoko no-repl
func tabulateVar<X>(size : Nat, generator : Nat -> X) : [var X]
```

Create a mutable array of size `size`. Each element at index i
is created by applying `generator` to i.

```motoko include=import
let array : [var Nat] = Array.tabulateVar<Nat>(4, func i = i * 2);
array[2] := 0;
array
```

Runtime: O(size)
Space: O(size)

*Runtime and space assumes that `generator` runs in O(1) time and space.


## Function `freeze`
``` motoko no-repl
func freeze<X>(varArray : [var X]) : [X]
```

Transforms a mutable array into an immutable array.

```motoko include=import

let varArray = [var 0, 1, 2];
varArray[2] := 3;
let array = Array.freeze<Nat>(varArray);
```

Runtime: O(size)

Space: O(1)


## Function `thaw`
``` motoko no-repl
func thaw<A>(array : [A]) : [var A]
```

Transforms an immutable array into a mutable array.

```motoko include=import

let array = [0, 1, 2];
let varArray = Array.thaw<Nat>(array);
varArray[2] := 3;
varArray
```

Runtime: O(size)

Space: O(1)


## Function `equal`
``` motoko no-repl
func equal<X>(array1 : [X], array2 : [X], equal : (X, X) -> Bool) : Bool
```

Tests if two arrays contain equal values (i.e. they represent the same
list of elements). Uses `equal` to compare elements in the arrays.

```motoko include=import
// Use the equal function from the Nat module to compare Nats
import {equal} "mo:base/Nat";

let array1 = [0, 1, 2, 3];
let array2 = [0, 1, 2, 3];
Array.equal(array1, array2, equal)
```

Runtime: O(size1 + size2)

Space: O(1)

*Runtime and space assumes that `equal` runs in O(1) time and space.


## Function `find`
``` motoko no-repl
func find<X>(array : [X], predicate : X -> Bool) : ?X
```

Returns the first value in `array` for which `predicate` returns true.
If no element satisfies the predicate, returns null.

```motoko include=import
let array = [1, 9, 4, 8];
Array.find<Nat>(array, func x = x > 8)
```
Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `predicate` runs in O(1) time and space.


## Function `append`
``` motoko no-repl
func append<X>(array1 : [X], array2 : [X]) : [X]
```

Create a new array by appending the values of `array1` and `array2`.
@deprecated `Array.append` copies its arguments and has linear complexity;
when used in a loop, consider using a `Buffer`, and `Buffer.append`, instead.

```motoko include=import
let array1 = [1, 2, 3];
let array2 = [4, 5, 6];
Array.append<Nat>(array1, array2)
```
Runtime: O(size1 + size2)

Space: O(size1 + size2)


## Function `sort`
``` motoko no-repl
func sort<X>(array : [X], compare : (X, X) -> Order.Order) : [X]
```

Sorts the elements in the array according to `compare`.
Sort is deterministic and stable.

```motoko include=import
import Nat "mo:base/Nat";

let array = [4, 2, 6];
Array.sort(array, Nat.compare)
```
Runtime: O(size * log(size))

Space: O(size)
*Runtime and space assumes that `compare` runs in O(1) time and space.


## Function `sortInPlace`
``` motoko no-repl
func sortInPlace<X>(array : [var X], compare : (X, X) -> Order.Order)
```

Sorts the elements in the array, __in place__, according to `compare`.
Sort is deterministic, stable, and in-place.

```motoko include=import

import {compare} "mo:base/Nat";

let array = [var 4, 2, 6];
Array.sortInPlace(array, compare);
array
```
Runtime: O(size * log(size))

Space: O(size)
*Runtime and space assumes that `compare` runs in O(1) time and space.


## Function `reverse`
``` motoko no-repl
func reverse<X>(array : [X]) : [X]
```

Creates a new array by reversing the order of elements in `array`.

```motoko include=import

let array = [10, 11, 12];

Array.reverse(array)
```

Runtime: O(size)

Space: O(1)


## Function `map`
``` motoko no-repl
func map<X, Y>(array : [X], f : X -> Y) : [Y]
```

Creates a new array by applying `f` to each element in `array`. `f` "maps"
each element it is applied to of type `X` to an element of type `Y`.
Retains original ordering of elements.

```motoko include=import

let array = [0, 1, 2, 3];
Array.map<Nat, Nat>(array, func x = x * 3)
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.


## Function `filter`
``` motoko no-repl
func filter<X>(array : [X], predicate : X -> Bool) : [X]
```

Creates a new array by applying `predicate` to every element
in `array`, retaining the elements for which `predicate` returns true.

```motoko include=import
let array = [4, 2, 6, 1, 5];
let evenElements = Array.filter<Nat>(array, func x = x % 2 == 0);
```
Runtime: O(size)

Space: O(size)
*Runtime and space assumes that `predicate` runs in O(1) time and space.


## Function `mapEntries`
``` motoko no-repl
func mapEntries<X, Y>(array : [X], f : (X, Nat) -> Y) : [Y]
```

Creates a new array by applying `f` to each element in `array` and its index.
Retains original ordering of elements.

```motoko include=import

let array = [10, 10, 10, 10];
Array.mapEntries<Nat, Nat>(array, func (i, x) = i * x)
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.


## Function `mapFilter`
``` motoko no-repl
func mapFilter<X, Y>(array : [X], f : X -> ?Y) : [Y]
```

Creates a new array by applying `f` to each element in `array`,
and keeping all non-null elements. The ordering is retained.

```motoko include=import
import {toText} "mo:base/Nat";

let array = [4, 2, 0, 1];
let newArray =
  Array.mapFilter<Nat, Text>( // mapping from Nat to Text values
    array,
    func x = if (x == 0) { null } else { ?toText(100 / x) } // can't divide by 0, so return null
  );
```
Runtime: O(size)

Space: O(size)
*Runtime and space assumes that `f` runs in O(1) time and space.


## Function `mapResult`
``` motoko no-repl
func mapResult<X, Y, E>(array : [X], f : X -> Result.Result<Y, E>) : Result.Result<[Y], E>
```

Creates a new array by applying `f` to each element in `array`.
If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
returns an `#ok` containing the new array.

```motoko include=import
let array = [4, 3, 2, 1, 0];
// divide 100 by every element in the array
Array.mapResult<Nat, Nat, Text>(array, func x {
  if (x > 0) {
    #ok(100 / x)
  } else {
    #err "Cannot divide by zero"
  }
})
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.


## Function `chain`
``` motoko no-repl
func chain<X, Y>(array : [X], k : X -> [Y]) : [Y]
```

Creates a new array by applying `k` to each element in `array`,
and concatenating the resulting arrays in order. This operation
is similar to what in other functional languages is known as monadic bind.

```motoko include=import
import Nat "mo:base/Nat";

let array = [1, 2, 3, 4];
Array.chain<Nat, Int>(array, func x = [x, -x])

```
Runtime: O(size)

Space: O(size)
*Runtime and space assumes that `k` runs in O(1) time and space.


## Function `foldLeft`
``` motoko no-repl
func foldLeft<X, A>(array : [X], base : A, combine : (A, X) -> A) : A
```

Collapses the elements in `array` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

```motoko include=import
import {add} "mo:base/Nat";

let array = [4, 2, 0, 1];
let sum =
  Array.foldLeft<Nat, Nat>(
    array,
    0, // start the sum at 0
    func(sumSoFar, x) = sumSoFar + x // this entire function can be replaced with `add`!
  );
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `combine` runs in O(1) time and space.


## Function `foldRight`
``` motoko no-repl
func foldRight<X, A>(array : [X], base : A, combine : (X, A) -> A) : A
```

Collapses the elements in `array` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
right to left.

```motoko include=import
import {toText} "mo:base/Nat";

let array = [1, 9, 4, 8];
let bookTitle = Array.foldRight<Nat, Text>(array, "", func(x, acc) = toText(x) # acc);
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `combine` runs in O(1) time and space.


## Function `flatten`
``` motoko no-repl
func flatten<X>(arrays : [[X]]) : [X]
```

Flattens the array of arrays into a single array. Retains the original
ordering of the elements.

```motoko include=import

let arrays = [[0, 1, 2], [2, 3], [], [4]];
Array.flatten<Nat>(arrays)
```

Runtime: O(number of elements in array)

Space: O(number of elements in array)


## Function `make`
``` motoko no-repl
func make<X>(element : X) : [X]
```

Create an array containing a single value.

```motoko include=import
Array.make(2)
```

Runtime: O(1)

Space: O(1)


## Function `vals`
``` motoko no-repl
func vals<X>(array : [X]) : I.Iter<X>
```

Returns an Iterator (`Iter`) over the elements of `array`.
Iterator provides a single method `next()`, which returns
elements in order, or `null` when out of elements to iterate over.

NOTE: You can also use `array.vals()` instead of this function. See example
below.

```motoko include=import

let array = [10, 11, 12];

var sum = 0;
for (element in array.vals()) {
  sum += element;
};
sum
```

Runtime: O(1)

Space: O(1)


## Function `keys`
``` motoko no-repl
func keys<X>(array : [X]) : I.Iter<Nat>
```

Returns an Iterator (`Iter`) over the indices of `array`.
Iterator provides a single method `next()`, which returns
indices in order, or `null` when out of index to iterate over.

NOTE: You can also use `array.keys()` instead of this function. See example
below.

```motoko include=import

let array = [10, 11, 12];

var sum = 0;
for (element in array.keys()) {
  sum += element;
};
sum
```

Runtime: O(1)

Space: O(1)

