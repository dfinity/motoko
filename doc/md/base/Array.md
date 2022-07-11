# Array
Functions on Arrays

## Function `equal`
`func equal<A>(a : [A], b : [A], eq : (A, A) -> Bool) : Bool`

Test if two arrays contain equal values

## Function `append`
`func append<A>(xs : [A], ys : [A]) : [A]`

Append the values of two input arrays
@deprecated `Array.append` copies its arguments and has linear complexity; when used in a loop, consider using a `Buffer`, and `Buffer.append`, instead.

## Function `sort`
`func sort<A>(xs : [A], cmp : (A, A) -> Order.Order) : [A]`

Sorts the given array according to the `cmp` function.
This is a _stable_ sort.

```motoko
import Array "mo:base/Array";
import Nat "mo:base/Nat";
let xs = [4, 2, 6];
assert(Array.sort(xs, Nat.compare) == [2, 4, 6])
```

## Function `sortInPlace`
`func sortInPlace<A>(xs : [var A], cmp : (A, A) -> Order.Order)`

Sorts the given array in place according to the `cmp` function.
This is a _stable_ sort.

```motoko
import Array "mo:base/Array";
import Nat "mo:base/Nat";
let xs : [var Nat] = [var 4, 2, 6, 1, 5];
Array.sortInPlace(xs, Nat.compare);
assert(Array.freeze(xs) == [1, 2, 4, 5, 6])
```

## Function `chain`
`func chain<A, B>(xs : [A], f : A -> [B]) : [B]`

Transform each array value into zero or more output values, appended in order

## Function `filter`
`func filter<A>(xs : [A], f : A -> Bool) : [A]`

Output array contains each array-value if and only if the predicate is true; ordering retained.

## Function `mapFilter`
`func mapFilter<A, B>(xs : [A], f : A -> ?B) : [B]`

Output array contains each transformed optional value; ordering retained.

## Function `foldLeft`
`func foldLeft<A, B>(xs : [A], initial : B, f : (B, A) -> B) : B`

Aggregate and transform values into a single output value, by increasing indices.

## Function `foldRight`
`func foldRight<A, B>(xs : [A], initial : B, f : (A, B) -> B) : B`

Aggregate and transform values into a single output value, by decreasing indices.

## Function `find`
`func find<A>(xs : [A], f : A -> Bool) : ?A`

Returns optional first value for which predicate is true

## Function `freeze`
`func freeze<A>(xs : [var A]) : [A]`

Transform mutable array into immutable array

## Function `flatten`
`func flatten<A>(xs : [[A]]) : [A]`

Transform an array of arrays into a single array, with retained array-value order.

## Function `map`
`func map<A, B>(xs : [A], f : A -> B) : [B]`

Transform each value using a function, with retained array-value order.

## Function `mapEntries`
`func mapEntries<A, B>(xs : [A], f : (A, Nat) -> B) : [B]`

Transform each entry (index-value pair) using a function.

## Function `mapResult`
`func mapResult<A, R, E>(xs : [A], f : A -> Result.Result<R, E>) : Result.Result<[R], E>`

Maps a Result-returning function over an Array and returns either
the first error or an array of successful values.

```motoko
import Array "mo:base/Array";
import Result "mo:base/Result";
import Int "mo:base/Int";
func makeNatural(x : Int) : Result.Result<Nat, Text> =
  if (x >= 0) {
    #ok(Int.abs(x))
  } else {
    #err(Int.toText(x) # " is not a natural number.")
  };

assert(Array.mapResult<Int, Nat, Text>([0, 1, 2], makeNatural) == #ok([0, 1, 2]));
assert(Array.mapResult([-1, 0, 1], makeNatural) == #err("-1 is not a natural number."));
```

## Function `make`
`func make<A>(x : A) : [A]`

Make an array from a single value.

## Function `vals`
`func vals<A>(xs : [A]) : I.Iter<A>`

Returns `xs.vals()`.

## Function `keys`
`func keys<A>(xs : [A]) : I.Iter<Nat>`

Returns `xs.keys()`.

## Function `thaw`
`func thaw<A>(xs : [A]) : [var A]`

Transform an immutable array into a mutable array.

## Function `init`
`func init<A>(size : Nat, initVal : A) : [var A]`

Initialize a mutable array with `size` copies of the initial value.

## Function `tabulate`
`func tabulate<A>(size : Nat, gen : Nat -> A) : [A]`

Initialize an immutable array of the given size, and use the `gen` function to produce the initial value for every index.

## Function `tabulateVar`
`func tabulateVar<A>(size : Nat, gen : Nat -> A) : [var A]`

Initialize a mutable array using a generation function

## Function `reverse`
`func reverse<A>(xs : [A]) : [A]`

