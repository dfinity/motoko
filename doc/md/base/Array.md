# Array
Functions on Arrays

## Function `equal`
``` motoko no-repl
func equal<A>(a : [A], b : [A], eq : (A, A) -> Bool) : Bool
```

Test if two arrays contain equal values

## Function `append`
``` motoko no-repl
func append<A>(xs : [A], ys : [A]) : [A]
```

Append the values of two input arrays
@deprecated `Array.append` copies its arguments and has linear complexity; when used in a loop, consider using a `Buffer`, and `Buffer.append`, instead.

## Function `sort`
``` motoko no-repl
func sort<A>(xs : [A], compare : (A, A) -> Order.Order) : [A]
```

Sorts the given array, in ascending order, according to the `compare` function.
This is a _stable_ sort.

```motoko
import Array "mo:base/Array";
import Nat "mo:base/Nat";
let xs = [4, 2, 6];
assert(Array.sort(xs, Nat.compare) == [2, 4, 6])
```

## Function `sortInPlace`
``` motoko no-repl
func sortInPlace<A>(xs : [var A], compare : (A, A) -> Order.Order)
```

Sorts the given array, in ascending order, in place, according to the `compare` function.
This is a _stable_ sort.

```motoko
import Array "mo:base/Array";
import Nat "mo:base/Nat";
let xs : [var Nat] = [var 4, 2, 6, 1, 5];
Array.sortInPlace(xs, Nat.compare);
assert(Array.freeze(xs) == [1, 2, 4, 5, 6])
```

## Function `chain`
``` motoko no-repl
func chain<A, B>(xs : [A], f : A -> [B]) : [B]
```

Transform each array value into zero or more output values, appended in order

## Function `filter`
``` motoko no-repl
func filter<A>(xs : [A], f : A -> Bool) : [A]
```

Output array contains each array-value if and only if the predicate is true; ordering retained.

## Function `mapFilter`
``` motoko no-repl
func mapFilter<A, B>(xs : [A], f : A -> ?B) : [B]
```

Output array contains each transformed optional value; ordering retained.

## Function `foldLeft`
``` motoko no-repl
func foldLeft<A, B>(xs : [A], initial : B, f : (B, A) -> B) : B
```

Aggregate and transform values into a single output value, by increasing indices.

## Function `foldRight`
``` motoko no-repl
func foldRight<A, B>(xs : [A], initial : B, f : (A, B) -> B) : B
```

Aggregate and transform values into a single output value, by decreasing indices.

## Function `find`
``` motoko no-repl
func find<A>(xs : [A], f : A -> Bool) : ?A
```

Returns optional first value for which predicate is true

## Function `freeze`
``` motoko no-repl
func freeze<A>(xs : [var A]) : [A]
```

Transform mutable array into immutable array

## Function `flatten`
``` motoko no-repl
func flatten<A>(xs : [[A]]) : [A]
```

Transform an array of arrays into a single array, with retained array-value order.

## Function `map`
``` motoko no-repl
func map<A, B>(xs : [A], f : A -> B) : [B]
```

Transform each value using a function, with retained array-value order.

## Function `mapEntries`
``` motoko no-repl
func mapEntries<A, B>(xs : [A], f : (A, Nat) -> B) : [B]
```

Transform each entry (index-value pair) using a function.

## Function `mapResult`
``` motoko no-repl
func mapResult<A, R, E>(xs : [A], f : A -> Result.Result<R, E>) : Result.Result<[R], E>
```

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
``` motoko no-repl
func make<A>(x : A) : [A]
```

Make an array from a single value.

## Function `vals`
``` motoko no-repl
func vals<A>(xs : [A]) : I.Iter<A>
```

Returns `xs.vals()`.

## Function `keys`
``` motoko no-repl
func keys<A>(xs : [A]) : I.Iter<Nat>
```

Returns `xs.keys()`.

## Function `thaw`
``` motoko no-repl
func thaw<A>(xs : [A]) : [var A]
```

Transform an immutable array into a mutable array.

## Function `init`
``` motoko no-repl
func init<A>(size : Nat, initVal : A) : [var A]
```

Initialize a mutable array with `size` copies of the initial value.

## Function `tabulate`
``` motoko no-repl
func tabulate<A>(size : Nat, gen : Nat -> A) : [A]
```

Initialize an immutable array of the given size, and use the `gen` function to produce the initial value for every index.

## Function `tabulateVar`
``` motoko no-repl
func tabulateVar<A>(size : Nat, gen : Nat -> A) : [var A]
```

Initialize a mutable array using a generation function

## Function `reverse`
``` motoko no-repl
func reverse<A>(xs : [A]) : [A]
```

