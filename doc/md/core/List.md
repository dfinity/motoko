# core/List
A mutable growable array data structure with efficient random access and dynamic resizing.
`List` provides O(1) access time and O(sqrt(n)) memory overhead. In contrast, `pure/List` is a purely functional linked list.
Can be declared `stable` for orthogonal persistence.

This implementation is adapted with permission from the `vector` Mops package created by Research AG.

Copyright: 2023 MR Research AG
Main author: Andrii Stepanov
Contributors: Timo Hanke (timohanke), Andy Gura (andygura), react0r-com

```motoko name=import
import List "mo:core/List";
```

## Type `List`
``` motoko no-repl
type List<T> = Types.List<T>
```

`List<T>` provides a mutable list of elements of type `T`.
Based on the paper "Resizable Arrays in Optimal Time and Space" by Brodnik, Carlsson, Demaine, Munro and Sedgewick (1999).
Since this is internally a two-dimensional array the access times for put and get operations
will naturally be 2x slower than Buffer and Array. However, Array is not resizable and Buffer
has `O(size)` memory waste.

The maximum number of elements in a `List` is 2^32.

## Function `empty`
``` motoko no-repl
func empty<T>() : List<T>
```

Creates a new empty List for elements of type T.

Example:
```motoko include=import
let list = List.empty<Nat>(); // Creates a new List
```

## Function `singleton`
``` motoko no-repl
func singleton<T>(element : T) : List<T>
```

Returns a new list with capacity and size 1, containing `element`.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.singleton<Nat>(1);
assert List.toText<Nat>(list, Nat.toText) == "List[1]";
```

Runtime: `O(1)`

Space: `O(1)`

## Function `repeat`
``` motoko no-repl
func repeat<T>(initValue : T, size : Nat) : List<T>
```

Creates a new List with `size` copies of the initial value.

Example:
```motoko include=import
let list = List.repeat<Nat>(2, 4);
assert List.toArray(list) == [2, 2, 2, 2];
```

Runtime: `O(size)`

Space: `O(size)`

## Function `toPure`
``` motoko no-repl
func toPure<T>(list : List<T>) : PureList.List<T>
```

Converts a mutable `List` to a purely functional `PureList`.

Example:
```motoko include=import
let list = List.fromArray<Nat>([1, 2, 3]);
let pureList = List.toPure<Nat>(list); // converts to immutable PureList
```

Runtime: `O(size)`

Space: `O(size)`

## Function `fromPure`
``` motoko no-repl
func fromPure<T>(pure : PureList.List<T>) : List<T>
```

Converts a purely functional `List` to a mutable `List`.

Example:
```motoko include=import
import PureList "mo:core/pure/List";

let pureList = PureList.fromArray<Nat>([1, 2, 3]);
let list = List.fromPure<Nat>(pureList); // converts to mutable List
```

Runtime: `O(size)`

Space: `O(size)`

## Function `addRepeat`
``` motoko no-repl
func addRepeat<T>(list : List<T>, initValue : T, count : Nat)
```

Add to list `count` copies of the initial value.

```motoko include=import
let list = List.repeat<Nat>(2, 4); // [2, 2, 2, 2]
List.addRepeat(list, 2, 1); // [2, 2, 2, 2, 1, 1]
```

The maximum number of elements in a `List` is 2^32.

Runtime: `O(count)`

## Function `clear`
``` motoko no-repl
func clear<T>(list : List<T>)
```

Resets the list to size 0, de-referencing all elements.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
List.add(list, 12);
List.clear(list); // list is now empty
assert List.toArray(list) == [];
```

Runtime: `O(1)`

## Function `clone`
``` motoko no-repl
func clone<T>(list : List<T>) : List<T>
```

Returns a copy of a List, with the same size.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 1);

let clone = List.clone(list);
assert List.toArray(clone) == [1];
```

Runtime: `O(size)`

## Function `map`
``` motoko no-repl
func map<T, R>(list : List<T>, f : T -> R) : List<R>
```

Creates a new list by applying the provided function to each element in the input list.
The resulting list has the same size as the input list.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.singleton<Nat>(123);
let textList = List.map<Nat, Text>(list, Nat.toText);
assert List.toArray(textList) == ["123"];
```

Runtime: `O(size)`

## Function `filter`
``` motoko no-repl
func filter<T>(list : List<T>, predicate : T -> Bool) : List<T>
```

Returns a new list containing only the elements from `list` for which the predicate returns true.

Example:
```motoko include=import
let list = List.fromArray<Nat>([1, 2, 3, 4]);
let evenNumbers = List.filter<Nat>(list, func x = x % 2 == 0);
assert List.toArray(evenNumbers) == [2, 4];
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `predicate` runs in `O(1)` time and space.

## Function `filterMap`
``` motoko no-repl
func filterMap<T, R>(list : List<T>, f : T -> ?R) : List<R>
```

Returns a new list containing all elements from `list` for which the function returns ?element.
Discards all elements for which the function returns null.

Example:
```motoko include=import
let list = List.fromArray<Nat>([1, 2, 3, 4]);
let doubled = List.filterMap<Nat, Nat>(list, func x = if (x % 2 == 0) ?(x * 2) else null);
assert List.toArray(doubled) == [4, 8];
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `f` runs in `O(1)` time and space.

## Function `size`
``` motoko no-repl
func size<T>(list : List<T>) : Nat
```

Returns the current number of elements in the list.

Example:
```motoko include=import
let list = List.empty<Nat>();
assert List.size(list) == 0
```

Runtime: `O(1)` (with some internal calculations)

## Function `add`
``` motoko no-repl
func add<T>(list : List<T>, element : T)
```

Adds a single element to the end of a List,
allocating a new internal data block if needed,
and resizing the internal index block if needed.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 0); // add 0 to list
List.add(list, 1);
List.add(list, 2);
List.add(list, 3);
assert List.toArray(list) == [0, 1, 2, 3];
```

The maximum number of elements in a `List` is 2^32.

Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`

## Function `removeLast`
``` motoko no-repl
func removeLast<T>(list : List<T>) : ?T
```

Removes and returns the last item in the list or `null` if
the list is empty.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
assert List.removeLast(list) == ?11;
assert List.removeLast(list) == ?10;
assert List.removeLast(list) == null;
```

Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`

Amortized Space: `O(1)`, Worst Case Space: `O(sqrt(n))`

## Function `at`
``` motoko no-repl
func at<T>(list : List<T>, index : Nat) : T
```

Returns the element at index `index`. Indexing is zero-based.
Traps if `index >= size`, error message may not be descriptive.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
assert List.at(list, 0) == 10;
```

Runtime: `O(1)`

## Function `get`
``` motoko no-repl
func get<T>(list : List<T>, index : Nat) : ?T
```

Returns the element at index `index` as an option.
Returns `null` when `index >= size`. Indexing is zero-based.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
assert List.get(list, 0) == ?10;
assert List.get(list, 2) == null;
```

Runtime: `O(1)`

Space: `O(1)`

## Function `put`
``` motoko no-repl
func put<T>(list : List<T>, index : Nat, value : T)
```

Overwrites the current element at `index` with `element`.
Traps if `index` >= size. Indexing is zero-based.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.put(list, 0, 20); // overwrites 10 at index 0 with 20
assert List.toArray(list) == [20];
```

Runtime: `O(1)`

## Function `sort`
``` motoko no-repl
func sort<T>(list : List<T>, compare : (T, T) -> Order.Order)
```

Sorts the elements in the list according to `compare`.
Sort is deterministic, stable, and in-place.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.empty<Nat>();
List.add(list, 3);
List.add(list, 1);
List.add(list, 2);
List.sort(list, Nat.compare);
assert List.toArray(list) == [1, 2, 3];
```

Runtime: O(size * log(size))

Space: O(size)
*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `indexOf`
``` motoko no-repl
func indexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat
```

Finds the first index of `element` in `list` using equality of elements defined
by `equal`. Returns `null` if `element` is not found.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.empty<Nat>();
List.add(list, 1);
List.add(list, 2);
List.add(list, 3);
List.add(list, 4);

assert List.indexOf<Nat>(list, Nat.equal, 3) == ?2;
assert List.indexOf<Nat>(list, Nat.equal, 5) == null;
```

Runtime: `O(size)`

*Runtime and space assumes that `equal` runs in `O(1)` time and space.

## Function `lastIndexOf`
``` motoko no-repl
func lastIndexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat
```

Finds the last index of `element` in `list` using equality of elements defined
by `equal`. Returns `null` if `element` is not found.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.fromArray<Nat>([1, 2, 3, 4, 2, 2]);

assert List.lastIndexOf<Nat>(list, Nat.equal, 2) == ?5;
assert List.lastIndexOf<Nat>(list, Nat.equal, 5) == null;
```

Runtime: `O(size)`

*Runtime and space assumes that `equal` runs in `O(1)` time and space.

## Function `find`
``` motoko no-repl
func find<T>(list : List<T>, predicate : T -> Bool) : ?T
```

Returns the first value in `list` for which `predicate` returns true.
If no element satisfies the predicate, returns null.

```motoko include=import
let list = List.fromArray<Nat>([1, 9, 4, 8]);
let found = List.find<Nat>(list, func(x) { x > 8 });
assert found == ?9;
```
Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `findIndex`
``` motoko no-repl
func findIndex<T>(list : List<T>, predicate : T -> Bool) : ?Nat
```

Finds the index of the first element in `list` for which `predicate` is true.
Returns `null` if no such element is found.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 1);
List.add(list, 2);
List.add(list, 3);
List.add(list, 4);

assert List.findIndex<Nat>(list, func(i) { i % 2 == 0 }) == ?1;
assert List.findIndex<Nat>(list, func(i) { i > 5 }) == null;
```

Runtime: `O(size)`

*Runtime and space assumes that `predicate` runs in `O(1)` time and space.

## Function `findLastIndex`
``` motoko no-repl
func findLastIndex<T>(list : List<T>, predicate : T -> Bool) : ?Nat
```

Finds the index of the last element in `list` for which `predicate` is true.
Returns `null` if no such element is found.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 1);
List.add(list, 2);
List.add(list, 3);
List.add(list, 4);

assert List.findLastIndex<Nat>(list, func(i) { i % 2 == 0 }) == ?3;
assert List.findLastIndex<Nat>(list, func(i) { i > 5 }) == null;
```

Runtime: `O(size)`

*Runtime and space assumes that `predicate` runs in `O(1)` time and space.

## Function `binarySearch`
``` motoko no-repl
func binarySearch<T>(list : List<T>, compare : (T, T) -> Order.Order, element : T) : {#found : Nat; #insertionIndex : Nat}
```

Performs binary search on a sorted list to find the index of the `element`.
Returns `#found(index)` if the element is found, or `#insertionIndex(index)` with the index
where the element would be inserted according to the ordering if not found.

If there are multiple equal elements, no guarantee is made about which index is returned.
The list must be sorted in ascending order according to the `compare` function.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.fromArray<Nat>([1, 3, 5, 7, 9, 11]);
assert List.binarySearch<Nat>(list, Nat.compare, 5) == #found(2);
assert List.binarySearch<Nat>(list, Nat.compare, 6) == #insertionIndex(3);
```

Runtime: `O(log(size))`

Space: `O(1)`

*Runtime and space assumes that `compare` runs in `O(1)` time and space.

## Function `all`
``` motoko no-repl
func all<T>(list : List<T>, predicate : T -> Bool) : Bool
```

Returns true iff every element in `list` satisfies `predicate`.
In particular, if `list` is empty the function returns `true`.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 2);
List.add(list, 3);
List.add(list, 4);

assert List.all<Nat>(list, func x { x > 1 });
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `any`
``` motoko no-repl
func any<T>(list : List<T>, predicate : T -> Bool) : Bool
```

Returns true iff some element in `list` satisfies `predicate`.
In particular, if `list` is empty the function returns `false`.

Example:
```motoko include=import
let list = List.empty<Nat>();
List.add(list, 2);
List.add(list, 3);
List.add(list, 4);

assert List.any<Nat>(list, func x { x > 3 });
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `predicate` runs in O(1) time and space.

## Function `values`
``` motoko no-repl
func values<T>(list : List<T>) : Iter.Iter<T>
```

Returns an Iterator (`Iter`) over the elements of a List.
Iterator provides a single method `next()`, which returns
elements in order, or `null` when out of elements to iterate over.

```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
List.add(list, 12);

var sum = 0;
for (element in List.values(list)) {
  sum += element;
};
assert sum == 33;
```

Note: This does not create a snapshot. If the returned iterator is not consumed at once,
and instead the consumption of the iterator is interleaved with other operations on the
List, then this may lead to unexpected results.

Runtime: `O(1)`

## Function `enumerate`
``` motoko no-repl
func enumerate<T>(list : List<T>) : Iter.Iter<(Nat, T)>
```

Returns an Iterator (`Iter`) over the items (index-value pairs) in the list.
Each item is a tuple of `(index, value)`. The iterator provides a single method
`next()` which returns elements in order, or `null` when out of elements.

```motoko include=import
import Iter "mo:core/Iter";

let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
List.add(list, 12);
assert Iter.toArray(List.enumerate(list)) == [(0, 10), (1, 11), (2, 12)];
```

Note: This does not create a snapshot. If the returned iterator is not consumed at once,
and instead the consumption of the iterator is interleaved with other operations on the
List, then this may lead to unexpected results.

Runtime: `O(1)`

Warning: Allocates memory on the heap to store ?(Nat, T).

## Function `reverseValues`
``` motoko no-repl
func reverseValues<T>(list : List<T>) : Iter.Iter<T>
```

Returns an Iterator (`Iter`) over the elements of the list in reverse order.
The iterator provides a single method `next()` which returns elements from
last to first, or `null` when out of elements.

```motoko include=import
let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
List.add(list, 12);

var sum = 0;
for (element in List.reverseValues(list)) {
  sum += element;
};
assert sum == 33;
```

Note: This does not create a snapshot. If the returned iterator is not consumed at once,
and instead the consumption of the iterator is interleaved with other operations on the
List, then this may lead to unexpected results.

Runtime: `O(1)`

## Function `reverseEnumerate`
``` motoko no-repl
func reverseEnumerate<T>(list : List<T>) : Iter.Iter<(Nat, T)>
```

Returns an Iterator (`Iter`) over the items in reverse order, i.e. pairs of index and value.
Iterator provides a single method `next()`, which returns
elements in reverse order, or `null` when out of elements to iterate over.

```motoko include=import
import Iter "mo:core/Iter";

let list = List.empty<Nat>();
List.add(list, 10);
List.add(list, 11);
List.add(list, 12);
assert Iter.toArray(List.reverseEnumerate(list)) == [(2, 12), (1, 11), (0, 10)];
```

Note: This does not create a snapshot. If the returned iterator is not consumed at once,
and instead the consumption of the iterator is interleaved with other operations on the
List, then this may lead to unexpected results.

Runtime: `O(1)`

Warning: Allocates memory on the heap to store ?(T, Nat).

## Function `keys`
``` motoko no-repl
func keys<T>(list : List<T>) : Iter.Iter<Nat>
```

Returns an Iterator (`Iter`) over the indices (keys) of the list.
The iterator provides a single method `next()` which returns indices
from 0 to size-1, or `null` when out of elements.

```motoko include=import
import Iter "mo:core/Iter";

let list = List.empty<Text>();
List.add(list, "A");
List.add(list, "B");
List.add(list, "C");
Iter.toArray(List.keys(list)) // [0, 1, 2]
```

Note: This does not create a snapshot. If the returned iterator is not consumed at once,
and instead the consumption of the iterator is interleaved with other operations on the
List, then this may lead to unexpected results.

Runtime: `O(1)`

## Function `fromIter`
``` motoko no-repl
func fromIter<T>(iter : Iter.Iter<T>) : List<T>
```

Creates a new List containing all elements from the provided iterator.
Elements are added in the order they are returned by the iterator.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let array = [1, 1, 1];
let iter = array.vals();

let list = List.fromIter<Nat>(iter);
assert Iter.toArray(List.values(list)) == [1, 1, 1];
```

Runtime: `O(size)`

## Function `addAll`
``` motoko no-repl
func addAll<T>(list : List<T>, iter : Iter.Iter<T>)
```

Adds all elements from the provided iterator to the end of the list.
Elements are added in the order they are returned by the iterator.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let array = [1, 1, 1];
let iter = array.vals();
let list = List.repeat<Nat>(2, 1);

List.addAll<Nat>(list, iter);
assert Iter.toArray(List.values(list)) == [2, 1, 1, 1];
```

The maximum number of elements in a `List` is 2^32.

Runtime: `O(size)`, where n is the size of iter.

## Function `toArray`
``` motoko no-repl
func toArray<T>(list : List<T>) : [T]
```

Creates a new immutable array containing all elements from the list.
Elements appear in the same order as in the list.

Example:
```motoko include=import
let list = List.fromArray<Nat>([1, 2, 3]);

assert List.toArray<Nat>(list) == [1, 2, 3];
```

Runtime: `O(size)`

## Function `fromArray`
``` motoko no-repl
func fromArray<T>(array : [T]) : List<T>
```

Creates a List containing elements from an Array.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let array = [2, 3];
let list = List.fromArray<Nat>(array);
assert Iter.toArray(List.values(list)) == [2, 3];
```

Runtime: `O(size)`

## Function `toVarArray`
``` motoko no-repl
func toVarArray<T>(list : List<T>) : [var T]
```

Creates a new mutable array containing all elements from the list.
Elements appear in the same order as in the list.

Example:
```motoko include=import
import Array "mo:core/Array";

let list = List.fromArray<Nat>([1, 2, 3]);

let varArray = List.toVarArray<Nat>(list);
assert Array.fromVarArray(varArray) == [1, 2, 3];
```

Runtime: `O(size)`

## Function `fromVarArray`
``` motoko no-repl
func fromVarArray<T>(array : [var T]) : List<T>
```

Creates a new List containing all elements from the mutable array.
Elements appear in the same order as in the array.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let array = [var 2, 3];
let list = List.fromVarArray<Nat>(array);
assert Iter.toArray(List.values(list)) == [2, 3];
```

Runtime: `O(size)`

## Function `first`
``` motoko no-repl
func first<T>(list : List<T>) : ?T
```

Returns the first element of `list`, or `null` if the list is empty.

Example:
```motoko include=import
assert List.first(List.fromArray<Nat>([1, 2, 3])) == ?1;
assert List.first(List.empty<Nat>()) == null;
```

Runtime: `O(1)`

Space: `O(1)`

## Function `last`
``` motoko no-repl
func last<T>(list : List<T>) : ?T
```

Returns the last element of `list`, or `null` if the list is empty.

Example:
```motoko include=import
assert List.last(List.fromArray<Nat>([1, 2, 3])) == ?3;
assert List.last(List.empty<Nat>()) == null;
```

Runtime: `O(1)`

Space: `O(1)`

## Function `forEach`
``` motoko no-repl
func forEach<T>(list : List<T>, f : T -> ())
```

Applies `f` to each element in `list`.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";

let list = List.fromArray<Nat>([1, 2, 3]);

List.forEach<Nat>(list, func(x) {
  Debug.print(Nat.toText(x)); // prints each element in list
});
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `forEachEntry`
``` motoko no-repl
func forEachEntry<T>(list : List<T>, f : (Nat, T) -> ())
```

Applies `f` to each item `(i, x)` in `list` where `i` is the key
and `x` is the value.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";

let list = List.fromArray<Nat>([1, 2, 3]);

List.forEachEntry<Nat>(list, func (i,x) {
  // prints each item (i,x) in list
  Debug.print(Nat.toText(i) # Nat.toText(x));
});
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `reverseForEachEntry`
``` motoko no-repl
func reverseForEachEntry<T>(list : List<T>, f : (Nat, T) -> ())
```

Like `forEachEntryRev` but iterates through the list in reverse order,
from end to beginning.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";

let list = List.fromArray<Nat>([1, 2, 3]);

List.reverseForEachEntry<Nat>(list, func (i,x) {
  // prints each item (i,x) in list
  Debug.print(Nat.toText(i) # Nat.toText(x));
});
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `reverseForEach`
``` motoko no-repl
func reverseForEach<T>(list : List<T>, f : T -> ())
```

Applies `f` to each element in `list` in reverse order.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Debug "mo:core/Debug";

let list = List.fromArray<Nat>([1, 2, 3]);

List.reverseForEach<Nat>(list, func (x) {
  Debug.print(Nat.toText(x)); // prints each element in list in reverse order
});
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `contains`
``` motoko no-repl
func contains<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : Bool
```

Returns true if the list contains the specified element according to the provided
equality function. Uses the provided `equal` function to compare elements.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.empty<Nat>();
List.add(list, 2);
List.add(list, 0);
List.add(list, 3);

assert List.contains<Nat>(list, Nat.equal, 2);
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `max`
``` motoko no-repl
func max<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T
```

Returns the greatest element in the list according to the ordering defined by `compare`.
Returns `null` if the list is empty.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.empty<Nat>();
List.add(list, 1);
List.add(list, 2);

assert List.max<Nat>(list, Nat.compare) == ?2;
assert List.max<Nat>(List.empty<Nat>(), Nat.compare) == null;
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `min`
``` motoko no-repl
func min<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T
```

Returns the least element in the list according to the ordering defined by `compare`.
Returns `null` if the list is empty.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.empty<Nat>();
List.add(list, 1);
List.add(list, 2);

assert List.min<Nat>(list, Nat.compare) == ?1;
assert List.min<Nat>(List.empty<Nat>(), Nat.compare) == null;
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `equal`
``` motoko no-repl
func equal<T>(list1 : List<T>, list2 : List<T>, equal : (T, T) -> Bool) : Bool
```

Tests if two lists are equal by comparing their elements using the provided `equal` function.
Returns true if and only if both lists have the same size and all corresponding elements
are equal according to the provided function.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list1 = List.fromArray<Nat>([1,2]);
let list2 = List.empty<Nat>();
List.add(list2, 1);
List.add(list2, 2);

assert List.equal<Nat>(list1, list2, Nat.equal);
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `equal` runs in O(1) time and space.

## Function `compare`
``` motoko no-repl
func compare<T>(list1 : List<T>, list2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order
```

Compares two lists lexicographically using the provided `compare` function.
Elements are compared pairwise until a difference is found or one list ends.
If all elements compare equal, the shorter list is considered less than the longer list.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list1 = List.fromArray<Nat>([0, 1]);
let list2 = List.fromArray<Nat>([2]);
let list3 = List.fromArray<Nat>([0, 1, 2]);

assert List.compare<Nat>(list1, list2, Nat.compare) == #less;
assert List.compare<Nat>(list1, list3, Nat.compare) == #less;
assert List.compare<Nat>(list2, list3, Nat.compare) == #greater;
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `compare` runs in O(1) time and space.

## Function `toText`
``` motoko no-repl
func toText<T>(list : List<T>, f : T -> Text) : Text
```

Creates a textual representation of `list`, using `toText` to recursively
convert the elements into Text.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.fromArray<Nat>([1,2,3,4]);

assert List.toText<Nat>(list, Nat.toText) == "List[1, 2, 3, 4]";
```

Runtime: `O(size)`

Space: `O(size)`

*Runtime and space assumes that `toText` runs in O(1) time and space.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<A, T>(list : List<T>, base : A, combine : (A, T) -> A) : A
```

Collapses the elements in `list` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.fromArray<Nat>([1,2,3]);

assert List.foldLeft<Text, Nat>(list, "", func (acc, x) { acc # Nat.toText(x)}) == "123";
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `combine` runs in O(1)` time and space.

## Function `foldRight`
``` motoko no-repl
func foldRight<T, A>(list : List<T>, base : A, combine : (T, A) -> A) : A
```

Collapses the elements in `list` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko include=import
import Nat "mo:core/Nat";

let list = List.fromArray<Nat>([1,2,3]);

assert List.foldRight<Nat, Text>(list, "", func (x, acc) { Nat.toText(x) # acc }) == "123";
```

Runtime: `O(size)`

Space: `O(1)`

*Runtime and space assumes that `combine` runs in O(1)` time and space.

## Function `reverseInPlace`
``` motoko no-repl
func reverseInPlace<T>(list : List<T>)
```

Reverses the order of elements in `list` by overwriting in place.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let list = List.fromArray<Nat>([1,2,3]);

List.reverseInPlace<Nat>(list);
assert Iter.toArray(List.values(list)) == [3, 2, 1];
```

Runtime: `O(size)`

Space: `O(1)`

## Function `reverse`
``` motoko no-repl
func reverse<T>(list : List<T>) : List<T>
```

Returns a new List with the elements from `list` in reverse order.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";

let list = List.fromArray<Nat>([1,2,3]);

let rlist = List.reverse<Nat>(list);
assert Iter.toArray(List.values(rlist)) == [3, 2, 1];
```

Runtime: `O(size)`

Space: `O(1)`

## Function `isEmpty`
``` motoko no-repl
func isEmpty<T>(list : List<T>) : Bool
```

Returns true if and only if the list is empty.

Example:
```motoko include=import
let list = List.fromArray<Nat>([2,0,3]);
assert not List.isEmpty<Nat>(list);
assert List.isEmpty<Nat>(List.empty<Nat>());
```

Runtime: `O(1)`

Space: `O(1)`
