# List
Purely-functional, singly-linked lists.
A list of type `List<T>` is either `null` or an optional pair of a value of type `T` and a tail, itself of type `List<T>`.

To use this library, import it using:

```motoko name=initialize
import List "mo:base/List";
```

## Type `List`
``` motoko no-repl
type List<T> = ?(T, List<T>)
```


## Function `nil`
``` motoko no-repl
func nil<T>() : List<T>
```

Create an empty list.

Example:
```motoko include=initialize
List.nil<Nat>() // => null
```

Runtime: O(1)

Space: O(1)

## Function `isNil`
``` motoko no-repl
func isNil<T>(l : List<T>) : Bool
```

Check whether a list is empty and return true if the list is empty.

Example:
```motoko include=initialize
List.isNil<Nat>(null) // => true
```

Runtime: O(1)

Space: O(1)

## Function `push`
``` motoko no-repl
func push<T>(x : T, l : List<T>) : List<T>
```

Add `x` to the head of `list`, and return the new list.

Example:
```motoko include=initialize
List.push<Nat>(0, null) // => ?(0, null);
```

Runtime: O(1)

Space: O(1)

## Function `last`
``` motoko no-repl
func last<T>(l : List<T>) : ?T
```

Return the last element of the list, if present.
Example:
```motoko include=initialize
List.last<Nat>(?(0, ?(1, null))) // => ?1
```

Runtime: O(size)

Space: O(1)

## Function `pop`
``` motoko no-repl
func pop<T>(l : List<T>) : (?T, List<T>)
```

Remove the head of the list, returning the optioned head and the tail of the list in a pair.
Returns `(null, null)` if the list is empty.

Example:
```motoko include=initialize
List.pop<Nat>(?(0, ?(1, null))) // => (?0, ?(1, null))
```

Runtime: O(1)

Space: O(1)

## Function `size`
``` motoko no-repl
func size<T>(l : List<T>) : Nat
```

Return the length of the list.

Example:
```motoko include=initialize
List.size<Nat>(?(0, ?(1, null))) // => 2
```

Runtime: O(size)

Space: O(1)

## Function `get`
``` motoko no-repl
func get<T>(l : List<T>, n : Nat) : ?T
```

Access any item in a list, zero-based.

NOTE: Indexing into a list is a linear operation, and usually an
indication that a list might not be the best data structure
to use.

Example:
```motoko include=initialize
List.get<Nat>(?(0, ?(1, null)), 1) // => ?1
```

Runtime: O(size)

Space: O(1)

## Function `reverse`
``` motoko no-repl
func reverse<T>(l : List<T>) : List<T>
```

Reverses the list.

Example:
```motoko include=initialize
List.reverse<Nat>(?(0, ?(1, ?(2, null)))) // => ?(2, ?(1, ?(0, null)))
```

Runtime: O(size)

Space: O(size)

## Function `iterate`
``` motoko no-repl
func iterate<T>(l : List<T>, f : T -> ())
```

Call the given function for its side effect, with each list element in turn.

Example:
```motoko include=initialize
var sum = 0;
List.iterate<Nat>(?(0, ?(1, ?(2, null))), func n { sum += n });
sum // => 3
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `map`
``` motoko no-repl
func map<T, U>(l : List<T>, f : T -> U) : List<U>
```

Call the given function `f` on each list element and collect the results
in a new list.

Example:
```motoko include=initialize
import Nat = "mo:base/Nat"
List.map<Nat, Text>(?(0, ?(1, ?(2, null))), Nat.toText) // => ?("0", ?("1", ?("2", null))
```

Runtime: O(size)

Space: O(size)
*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `filter`
``` motoko no-repl
func filter<T>(l : List<T>, f : T -> Bool) : List<T>
```

Create a new list with only those elements of the original list for which
the given function (often called the _predicate_) returns true.

Example:
```motoko include=initialize
List.filter<Nat>(?(0, ?(1, ?(2, null))), func n { n != 1 }) // => ?(0, ?(2, null))
```

Runtime: O(size)

Space: O(size)

## Function `partition`
``` motoko no-repl
func partition<T>(l : List<T>, f : T -> Bool) : (List<T>, List<T>)
```

Create two new lists from the results of a given function (`f`).
The first list only includes the elements for which the given
function `f` returns true and the second list only includes
the elements for which the function returns false.

Example:
```motoko include=initialize
List.partition<Nat>(?(0, ?(1, ?(2, null))), func n { n != 1 }) // => (?(0, ?(2, null)), ?(1, null))
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<T, U>(l : List<T>, f : T -> ?U) : List<U>
```

Call the given function on each list element, and collect the non-null results
in a new list.

Example:
```motoko include=initialize
List.mapFilter<Nat, Nat>(
  ?(1, ?(2, ?(3, null))),
  func n {
    if (n > 1) {
      ?(n * 2);
    } else {
      null
    }
  }
) // => ?(4, ?(6, null))
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `mapResult`
``` motoko no-repl
func mapResult<T, R, E>(xs : List<T>, f : T -> Result.Result<R, E>) : Result.Result<List<R>, E>
```

Maps a Result-returning function `f` over a List and returns either
the first error or a list of successful values.

Example:
```motoko include=initialize
List.mapResult<Nat, Nat, Text>(
  ?(1, ?(2, ?(3, null))),
  func n {
    if (n > 0) {
      #ok(n * 2);
    } else {
      #err("Some element is zero")
    }
  }
); // => #ok ?(2, ?(4, ?(6, null))
```

Runtime: O(size)

Space: O(size)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `append`
``` motoko no-repl
func append<T>(l : List<T>, m : List<T>) : List<T>
```

Append the elements from one list to another list.

Example:
```motoko include=initialize
List.append<Nat>(
  ?(0, ?(1, ?(2, null))),
  ?(3, ?(4, ?(5, null)))
) // => ?(0, ?(1, ?(2, ?(3, ?(4, ?(5, null))))))
```

Runtime: O(size(l))

Space: O(size(l))

## Function `flatten`
``` motoko no-repl
func flatten<T>(l : List<List<T>>) : List<T>
```

Flatten, or concatenate, a list of lists as a list.

Example:
```motoko include=initialize
List.flatten<Nat>(
  ?(?(0, ?(1, ?(2, null))),
    ?(?(3, ?(4, ?(5, null))),
      null))
); // => ?(0, ?(1, ?(2, ?(3, ?(4, ?(5, null))))))
```

Runtime: O(size*size)

Space: O(size*size)

## Function `take`
``` motoko no-repl
func take<T>(l : List<T>, n : Nat) : List<T>
```

Returns the first `n` elements of the given list.
If the given list has fewer than `n` elements, this function returns
a copy of the full input list.

Example:
```motoko include=initialize
List.take<Nat>(
  ?(0, ?(1, ?(2, null))),
  2
); // => ?(0, ?(1, null))
```

Runtime: O(n)

Space: O(n)

## Function `drop`
``` motoko no-repl
func drop<T>(l : List<T>, n : Nat) : List<T>
```

Drop the first `n` elements from the given list.

Example:
```motoko include=initialize
List.drop<Nat>(
  ?(0, ?(1, ?(2, null))),
  2
); // => ?(2, null)
```

Runtime: O(n)

Space: O(1)

## Function `foldLeft`
``` motoko no-repl
func foldLeft<T, S>(list : List<T>, base : S, combine : (S, T) -> S) : S
```

Collapses the elements in `list` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
left to right.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

List.foldLeft<Nat, Text>(
  ?(1, ?(2, ?(3, null))),
  "",
  func (acc, x) { acc # Nat.toText(x)}
) // => "123"
```

Runtime: O(size(list))

Space: O(1) heap, O(1) stack

*Runtime and space assumes that `combine` runs in O(1) time and space.

## Function `foldRight`
``` motoko no-repl
func foldRight<T, S>(list : List<T>, base : S, combine : (T, S) -> S) : S
```

Collapses the elements in `buffer` into a single value by starting with `base`
and progessively combining elements into `base` with `combine`. Iteration runs
right to left.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

List.foldRight<Nat, Text>(
  ?(1, ?(2, ?(3, null))),
  "",
  func (x, acc) { Nat.toText(x) # acc}
) // => "123"
```

Runtime: O(size(list))

Space: O(1) heap, O(size(list)) stack

*Runtime and space assumes that `combine` runs in O(1) time and space.

## Function `find`
``` motoko no-repl
func find<T>(l : List<T>, f : T -> Bool) : ?T
```

Return the first element for which the given predicate `f` is true,
if such an element exists.

Example:
```motoko include=initialize

List.find<Nat>(
  ?(1, ?(2, ?(3, null))),
  func n { n > 1 }
); // => ?2
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `some`
``` motoko no-repl
func some<T>(l : List<T>, f : T -> Bool) : Bool
```

Return true if there exists a list element for which
the given predicate `f` is true.

Example:
```motoko include=initialize

List.some<Nat>(
  ?(1, ?(2, ?(3, null))),
  func n { n > 1 }
) // => true
```

Runtime: O(size(list))

Space: O(1)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `all`
``` motoko no-repl
func all<T>(l : List<T>, f : T -> Bool) : Bool
```

Return true if the given predicate `f` is true for all list
elements.

Example:
```motoko include=initialize

List.all<Nat>(
  ?(1, ?(2, ?(3, null))),
  func n { n > 1 }
); // => false
```

Runtime: O(size)

Space: O(1)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `merge`
``` motoko no-repl
func merge<T>(l1 : List<T>, l2 : List<T>, lessThanOrEqual : (T, T) -> Bool) : List<T>
```

Merge two ordered lists into a single ordered list.
This function requires both list to be ordered as specified
by the given relation `lessThanOrEqual`.

Example:
```motoko include=initialize

List.merge<Nat>(
  ?(1, ?(2, ?(4, null))),
  ?(2, ?(4, ?(6, null))),
  func (n1, n2) { n1 <= n2 }
); // => ?(1, ?(2, ?(2, ?(4, ?(4, ?(6, null))))))),
```

Runtime: O(size(l1) + size(l2))

Space: O(size(l1) + size(l2))

*Runtime and space assumes that `lessThanOrEqual` runs in O(1) time and space.

## Function `compare`
``` motoko no-repl
func compare<T>(l1 : List<T>, l2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order
```

Compare two lists using lexicographic ordering specified by argument function `compare`.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

List.compare<Nat>(
  ?(1, ?(2, null)),
  ?(3, ?(4, null)),
  Nat.compare
) // => #less
```

Runtime: O(size(l1))

Space: O(1)

*Runtime and space assumes that argument `compare` runs in O(1) time and space.

## Function `equal`
``` motoko no-repl
func equal<T>(l1 : List<T>, l2 : List<T>, equal : (T, T) -> Bool) : Bool
```

Compare two lists for equality using the argument function `equal` to determine equality of their elements.

Example:
```motoko include=initialize
import Nat "mo:base/Nat";

List.equal<Nat>(
  ?(1, ?(2, null)),
  ?(3, ?(4, null)),
  Nat.equal
); // => false
```

Runtime: O(size(l1))

Space: O(1)

*Runtime and space assumes that argument `equal` runs in O(1) time and space.

## Function `tabulate`
``` motoko no-repl
func tabulate<T>(n : Nat, f : Nat -> T) : List<T>
```

Generate a list based on a length and a function that maps from
a list index to a list element.

Example:
```motoko include=initialize
List.tabulate<Nat>(
  3,
  func n { n * 2 }
) // => ?(0, ?(2, (?4, null)))
```

Runtime: O(n)

Space: O(n)

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `make`
``` motoko no-repl
func make<T>(x : T) : List<T>
```

Create a list with exactly one element.

Example:
```motoko include=initialize
List.make<Nat>(
  0
) // => ?(0, null)
```

Runtime: O(1)

Space: O(1)

## Function `replicate`
``` motoko no-repl
func replicate<T>(n : Nat, x : T) : List<T>
```

Create a list of the given length with the same value in each position.

Example:
```motoko include=initialize
List.replicate<Nat>(
  3,
  0
) // => ?(0, ?(0, ?(0, null)))
```

Runtime: O(n)

Space: O(n)

## Function `zip`
``` motoko no-repl
func zip<T, U>(xs : List<T>, ys : List<U>) : List<(T, U)>
```

Create a list of pairs from a pair of lists.

If the given lists have different lengths, then the created list will have a
length equal to the length of the smaller list.

Example:
```motoko include=initialize
List.zip<Nat, Text>(
  ?(0, ?(1, ?(2, null))),
  ?("0", ?("1", null)),
) // => ?((0, "0"), ?((1, "1"), null))
```

Runtime: O(min(size(xs), size(ys)))

Space: O(min(size(xs), size(ys)))

## Function `zipWith`
``` motoko no-repl
func zipWith<T, U, V>(xs : List<T>, ys : List<U>, f : (T, U) -> V) : List<V>
```

Create a list in which elements are created by applying function `f` to each pair `(x, y)` of elements
occuring at the same position in list `xs` and list `ys`.

If the given lists have different lengths, then the created list will have a
length equal to the length of the smaller list.

Example:
```motoko include=initialize
import Nat = "mo:base/Nat";
import Char = "mo:base/Char";

List.zipWith<Nat, Char, Text>(
  ?(0, ?(1, ?(2, null))),
  ?('a', ?('b', null)),
  func (n, c) { Nat.toText(n) # Char.toText(c) }
) // => ?("0a", ?("1b", null))
```

Runtime: O(min(size(xs), size(ys)))

Space: O(min(size(xs), size(ys)))

*Runtime and space assumes that `f` runs in O(1) time and space.

## Function `split`
``` motoko no-repl
func split<T>(n : Nat, xs : List<T>) : (List<T>, List<T>)
```

Split the given list at the given zero-based index.

Example:
```motoko include=initialize
List.split<Nat>(
  2,
  ?(0, ?(1, ?(2, null)))
) // => (?(0, ?(1, null)), ?(2, null))
```

Runtime: O(n)

Space: O(n)

## Function `chunks`
``` motoko no-repl
func chunks<T>(n : Nat, xs : List<T>) : List<List<T>>
```

Split the given list into chunks of length `n`.
The last chunk will be shorter if the length of the given list
does not divide by `n` evenly.

Example:
```motoko include=initialize
List.chunks<Nat>(
  2,
  ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
)
/* => ?(?(0, ?(1, null)),
        ?(?(2, ?(3, null)),
          ?(?(4, null),
            null)))
*/
```

Runtime: O(size)

Space: O(size)

## Function `fromArray`
``` motoko no-repl
func fromArray<T>(xs : [T]) : List<T>
```

Convert an array into a list.

Example:
```motoko include=initialize
List.fromArray<Nat>([ 0, 1, 2, 3, 4])
// =>  ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
```

Runtime: O(size)

Space: O(size)

## Function `fromVarArray`
``` motoko no-repl
func fromVarArray<T>(xs : [var T]) : List<T>
```

Convert a mutable array into a list.

Example:
```motoko include=initialize
List.fromVarArray<Nat>([var 0, 1, 2, 3, 4])
// =>  ?(0, ?(1, ?(2, ?(3, ?(4, null)))))
```

Runtime: O(size)

Space: O(size)

## Function `toArray`
``` motoko no-repl
func toArray<T>(xs : List<T>) : [T]
```

Create an array from a list.
Example:
```motoko include=initialize
List.toArray<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))
// => [0, 1, 2, 3, 4]
```

Runtime: O(size)

Space: O(size)

## Function `toVarArray`
``` motoko no-repl
func toVarArray<T>(xs : List<T>) : [var T]
```

Create a mutable array from a list.
Example:
```motoko include=initialize
List.toVarArray<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))
// => [var 0, 1, 2, 3, 4]
```

Runtime: O(size)

Space: O(size)

## Function `toIter`
``` motoko no-repl
func toIter<T>(xs : List<T>) : Iter.Iter<T>
```

Create an iterator from a list.
Example:
```motoko include=initialize
var sum = 0;
for (n in List.toIter<Nat>(?(0, ?(1, ?(2, ?(3, ?(4, null))))))) {
  sum += n;
};
sum
// => 10
```

Runtime: O(1)

Space: O(1)
