# List
Purely-functional, singly-linked lists.

## Type `List`
``` motoko norepl
type List<T> = ?(T, List<T>)
```


## Function `nil`
``` motoko norepl
func nil<T>() : List<T>
```

Create an empty list.

## Function `isNil`
``` motoko norepl
func isNil<T>(l : List<T>) : Bool
```

Check whether a list is empty and return true if the list is empty.

## Function `push`
``` motoko norepl
func push<T>(x : T, l : List<T>) : List<T>
```

Construct a list by pre-pending a value.
This function is similar to a `list.cons(item)` function.

## Function `last`
``` motoko norepl
func last<T>(l : List<T>) : ?T
```

Return the last element of the list, if present.

## Function `pop`
``` motoko norepl
func pop<T>(l : List<T>) : (?T, List<T>)
```

Treat the list as a stack.
This function combines the `head` and (non-failing) `tail` operations into one operation.

## Function `size`
``` motoko norepl
func size<T>(l : List<T>) : Nat
```

Return the length of the list.

## Function `get`
``` motoko norepl
func get<T>(l : List<T>, n : Nat) : ?T
```

Access any item in a list, zero-based.

NOTE: Indexing into a list is a linear operation, and usually an
indication that a list might not be the best data structure
to use.

## Function `reverse`
``` motoko norepl
func reverse<T>(l : List<T>) : List<T>
```

Reverses the list

## Function `iterate`
``` motoko norepl
func iterate<T>(l : List<T>, f : T -> ())
```

Call the given function with each list element in turn.

This function is equivalent to the `app` function in Standard ML Basis,
and the `iter` function in OCaml.

## Function `map`
``` motoko norepl
func map<T, S>(l : List<T>, f : T -> S) : List<S>
```

Call the given function on each list element and collect the results
in a new list.

## Function `filter`
``` motoko norepl
func filter<T>(l : List<T>, f : T -> Bool) : List<T>
```

Create a new list with only those elements of the original list for which
the given function (often called the _predicate_) returns true.

## Function `partition`
``` motoko norepl
func partition<T>(l : List<T>, f : T -> Bool) : (List<T>, List<T>)
```

Create two new lists from the results of a given function (`f`).
The first list only includes the elements for which the given
function `f` returns true and the second list only includes
the elements for which the function returns false.

## Function `mapFilter`
``` motoko norepl
func mapFilter<T, S>(l : List<T>, f : T -> ?S) : List<S>
```

Call the given function on each list element, and collect the non-null results
in a new list.

## Function `mapResult`
``` motoko norepl
func mapResult<A, R, E>(xs : List<A>, f : A -> Result.Result<R, E>) : Result.Result<List<R>, E>
```

Maps a Result-returning function over a List and returns either
the first error or a list of successful values.

## Function `append`
``` motoko norepl
func append<T>(l : List<T>, m : List<T>) : List<T>
```

Append the elements from one list to another list.

## Function `flatten`
``` motoko norepl
func flatten<T>(l : List<List<T>>) : List<T>
```

Concatenate a list of lists.

In some languages, this operation is also known as a `list join`.

## Function `take`
``` motoko norepl
func take<T>(l : List<T>, n : Nat) : List<T>
```

Returns the first `n` elements of the given list.
If the given list has fewer than `n` elements, this function returns
a copy of the full input list.

## Function `drop`
``` motoko norepl
func drop<T>(l : List<T>, n : Nat) : List<T>
```

Drop the first `n` elements from the given list.

## Function `foldLeft`
``` motoko norepl
func foldLeft<T, S>(l : List<T>, a : S, f : (S, T) -> S) : S
```

Fold the list left-to-right using the given function (`f`).

## Function `foldRight`
``` motoko norepl
func foldRight<T, S>(l : List<T>, a : S, f : (T, S) -> S) : S
```

Fold the list right-to-left using the given function (`f`).

## Function `find`
``` motoko norepl
func find<T>(l : List<T>, f : T -> Bool) : ?T
```

Return the first element for which the given predicate `f` is true,
if such an element exists.

## Function `some`
``` motoko norepl
func some<T>(l : List<T>, f : T -> Bool) : Bool
```

Return true if there exists a list element for which
the given predicate `f` is true.

## Function `all`
``` motoko norepl
func all<T>(l : List<T>, f : T -> Bool) : Bool
```

Return true if the given predicate `f` is true for all list
elements.

## Function `merge`
``` motoko norepl
func merge<T>(l1 : List<T>, l2 : List<T>, lte : (T, T) -> Bool) : List<T>
```

Merge two ordered lists into a single ordered list.
This function requires both list to be ordered as specified
by the given relation `lte`.

## Function `compare`
``` motoko norepl
func compare<T>(l1 : List<T>, l2 : List<T>, compElm : (T, T) -> Order.Order) : Order.Order
```

Compare two lists using lexicographic ordering specified by the given relation `lte`.

## Function `equal`
``` motoko norepl
func equal<T>(l1 : List<T>, l2 : List<T>, eq : (T, T) -> Bool) : Bool
```

Compare two lists for equality as specified by the given relation `eq` on the elements.

The function `isEq(l1, l2)` is equivalent to `lessThanEq(l1, l2) && lessThanEq(l2, l1)`,
but the former is more efficient.

## Function `tabulate`
``` motoko norepl
func tabulate<T>(n : Nat, f : Nat -> T) : List<T>
```

Generate a list based on a length and a function that maps from
a list index to a list element.

## Function `make`
``` motoko norepl
func make<X>(x : X) : List<X>
```

Create a list with exactly one element.

## Function `replicate`
``` motoko norepl
func replicate<X>(n : Nat, x : X) : List<X>
```

Create a list of the given length with the same value in each position.

## Function `zip`
``` motoko norepl
func zip<X, Y>(xs : List<X>, ys : List<Y>) : List<(X, Y)>
```

Create a list of pairs from a pair of lists.

If the given lists have different lengths, then the created list will have a
length equal to the length of the smaller list.

## Function `zipWith`
``` motoko norepl
func zipWith<X, Y, Z>(xs : List<X>, ys : List<Y>, f : (X, Y) -> Z) : List<Z>
```

Create a list in which elements are calculated from the function `f` and
include elements occuring at the same position in the given lists.

If the given lists have different lengths, then the created list will have a
length equal to the length of the smaller list.

## Function `split`
``` motoko norepl
func split<X>(n : Nat, xs : List<X>) : (List<X>, List<X>)
```

Split the given list at the given zero-based index.

## Function `chunks`
``` motoko norepl
func chunks<X>(n : Nat, xs : List<X>) : List<List<X>>
```

Split the given list into chunks of length `n`.
The last chunk will be shorter if the length of the given list
does not divide by `n` evenly.

## Function `fromArray`
``` motoko norepl
func fromArray<A>(xs : [A]) : List<A>
```

Convert an array into a list.

## Function `fromVarArray`
``` motoko norepl
func fromVarArray<A>(xs : [var A]) : List<A>
```

Convert a mutable array into a list.

## Function `toArray`
``` motoko norepl
func toArray<A>(xs : List<A>) : [A]
```

Create an array from a list.

## Function `toVarArray`
``` motoko norepl
func toVarArray<A>(xs : List<A>) : [var A]
```

Create a mutable array from a list.

## Function `toIter`
``` motoko norepl
func toIter<A>(xs : List<A>) : Iter.Iter<A>
```

Create an iterator from a list.
