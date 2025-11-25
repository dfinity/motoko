# core/Nat
Natural numbers with infinite precision.

Most operations on natural numbers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

Import from the core package to use this module.
```motoko name=import
import Nat "mo:core/Nat";
```

## Type `Nat`
``` motoko no-repl
type Nat = Prim.Types.Nat
```

Infinite precision natural numbers.

## Function `toText`
``` motoko no-repl
func toText(_ : Nat) : Text
```

Converts a natural number to its textual representation. Textual
representation _do not_ contain underscores to represent commas.

Example:
```motoko include=import
assert Nat.toText(1234) == "1234";
```

## Function `fromText`
``` motoko no-repl
func fromText(text : Text) : ?Nat
```

Creates a natural number from its textual representation. Returns `null`
if the input is not a valid natural number.

The textual representation _must not_ contain underscores.

Example:
```motoko include=import
assert Nat.fromText("1234") == ?1234;
```

## Function `fromInt`
``` motoko no-repl
func fromInt(int : Int) : Nat
```

Converts an integer to a natural number. Traps if the integer is negative.

Example:
```motoko include=import
assert Nat.fromInt(1234) == (1234 : Nat);
```

## Function `toInt`
``` motoko no-repl
func toInt(nat : Nat) : Int
```

Converts a natural number to an integer.

Example:
```motoko include=import
assert Nat.toInt(1234) == 1234;
```

## Function `min`
``` motoko no-repl
func min(x : Nat, y : Nat) : Nat
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
assert Nat.min(1, 2) == 1;
```

## Function `max`
``` motoko no-repl
func max(x : Nat, y : Nat) : Nat
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
assert Nat.max(1, 2) == 2;
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat, y : Nat) : Bool
```

Equality function for Nat types.
This is equivalent to `x == y`.

Example:
```motoko include=import
assert Nat.equal(1, 1);
assert 1 == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
let a = 111;
let b = 222;
assert not Nat.equal(a, b);
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat, y : Nat) : Bool
```

Inequality function for Nat types.
This is equivalent to `x != y`.

Example:
```motoko include=import
assert Nat.notEqual(1, 2);
assert 1 != 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Nat, y : Nat) : Bool
```

"Less than" function for Nat types.
This is equivalent to `x < y`.

Example:
```motoko include=import
assert Nat.less(1, 2);
assert 1 < 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat, y : Nat) : Bool
```

"Less than or equal" function for Nat types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
assert Nat.lessOrEqual(1, 2);
assert 1 <= 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Nat, y : Nat) : Bool
```

"Greater than" function for Nat types.
This is equivalent to `x > y`.

Example:
```motoko include=import
assert Nat.greater(2, 1);
assert 2 > 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat, y : Nat) : Bool
```

"Greater than or equal" function for Nat types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
assert Nat.greaterOrEqual(2, 1);
assert 2 >= 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat, y : Nat) : Order.Order
```

General purpose comparison function for `Nat`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
assert Nat.compare(2, 3) == #less;
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.sort([2, 3, 1], Nat.compare) == [1, 2, 3];
```

## Function `add`
``` motoko no-repl
func add(x : Nat, y : Nat) : Nat
```

Returns the sum of `x` and `y`, `x + y`. This operator will never overflow
because `Nat` is infinite precision.

Example:
```motoko include=import
assert Nat.add(1, 2) == 3;
assert 1 + 2 == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([2, 3, 1], 0, Nat.add) == 6;
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat, y : Nat) : Nat
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow below `0`.

Example:
```motoko include=import
assert Nat.sub(2, 1) == 1;
// Add a type annotation to avoid a warning about the subtraction
assert 2 - 1 : Nat == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([2, 3, 1], 10, Nat.sub) == 4;
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat, y : Nat) : Nat
```

Returns the product of `x` and `y`, `x * y`. This operator will never
overflow because `Nat` is infinite precision.

Example:
```motoko include=import
assert Nat.mul(2, 3) == 6;
assert 2 * 3 == 6;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([2, 3, 1], 1, Nat.mul) == 6;
```

## Function `div`
``` motoko no-repl
func div(x : Nat, y : Nat) : Nat
```

Returns the unsigned integer division of `x` by `y`,  `x / y`.
Traps when `y` is zero.

The quotient is rounded down, which is equivalent to truncating the
decimal places of the quotient.

Example:
```motoko include=import
assert Nat.div(6, 2) == 3;
assert 6 / 2 == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Nat, y : Nat) : Nat
```

Returns the remainder of unsigned integer division of `x` by `y`,  `x % y`.
Traps when `y` is zero.

Example:
```motoko include=import
assert Nat.rem(6, 4) == 2;
assert 6 % 4 == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Nat, y : Nat) : Nat
```

Returns `x` to the power of `y`, `x ** y`. Traps when `y > 2^32`. This operator
will never overflow because `Nat` is infinite precision.

Example:
```motoko include=import
assert Nat.pow(2, 3) == 8;
assert 2 ** 3 == 8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat, y : Nat32) : Nat
```

Returns the (conceptual) bitwise shift left of `x` by `y`, `x * (2 ** y)`.

Example:
```motoko include=import
assert Nat.bitshiftLeft(1, 3) == 8;
```

Note: The reason why this function is defined in this library (in absence
of the `<<` operator) is so that you can use it as a function
value to pass to a higher order function. While `Nat` is not defined in terms
of bit patterns, conceptually it can be regarded as such, and the operation
is provided as a high-performance version of the corresponding arithmetic
rule.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat, y : Nat32) : Nat
```

Returns the (conceptual) bitwise shift right of `x` by `y`, `x / (2 ** y)`.

Example:
```motoko include=import
assert Nat.bitshiftRight(8, 3) == 1;
```

Note: The reason why this function is defined in this library (in absence
of the `>>` operator) is so that you can use it as a function
value to pass to a higher order function. While `Nat` is not defined in terms
of bit patterns, conceptually it can be regarded as such, and the operation
is provided as a high-performance version of the corresponding arithmetic
rule.

## Function `range`
``` motoko no-repl
func range(fromInclusive : Nat, toExclusive : Nat) : Iter.Iter<Nat>
```

Returns an iterator over `Nat` values from the first to second argument with an exclusive upper bound.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat.range(1, 4);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat.range(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeBy`
``` motoko no-repl
func rangeBy(fromInclusive : Nat, toExclusive : Nat, step : Int) : Iter.Iter<Nat>
```

Returns an iterator over `Nat` values from the first to second argument with an exclusive upper bound,
incrementing by the specified step size. The step can be positive or negative.
```motoko include=import
import Iter "mo:core/Iter";

// Positive step
let iter1 = Nat.rangeBy(1, 7, 2);
assert iter1.next() == ?1;
assert iter1.next() == ?3;
assert iter1.next() == ?5;
assert iter1.next() == null;

// Negative step
let iter2 = Nat.rangeBy(7, 1, -2);
assert iter2.next() == ?7;
assert iter2.next() == ?5;
assert iter2.next() == ?3;
assert iter2.next() == null;
```

If `step` is 0 or if the iteration would not progress towards the bound, returns an empty iterator.

## Function `rangeInclusive`
``` motoko no-repl
func rangeInclusive(from : Nat, to : Nat) : Iter.Iter<Nat>
```

Returns an iterator over the integers from the first to second argument, inclusive.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat.rangeInclusive(1, 3);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat.rangeInclusive(3, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeByInclusive`
``` motoko no-repl
func rangeByInclusive(from : Nat, to : Nat, step : Int) : Iter.Iter<Nat>
```

Returns an iterator over the integers from the first to second argument, inclusive,
incrementing by the specified step size. The step can be positive or negative.
```motoko include=import
import Iter "mo:core/Iter";

// Positive step
let iter1 = Nat.rangeByInclusive(1, 7, 2);
assert iter1.next() == ?1;
assert iter1.next() == ?3;
assert iter1.next() == ?5;
assert iter1.next() == ?7;
assert iter1.next() == null;

// Negative step
let iter2 = Nat.rangeByInclusive(7, 1, -2);
assert iter2.next() == ?7;
assert iter2.next() == ?5;
assert iter2.next() == ?3;
assert iter2.next() == ?1;
assert iter2.next() == null;
```

If `from == to`, return an iterator which only returns that value.

Otherwise, if `step` is 0 or if the iteration would not progress towards the bound, returns an empty iterator.

## Function `allValues`
``` motoko no-repl
func allValues() : Iter.Iter<Nat>
```

Returns an infinite iterator over all possible `Nat` values.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat.allValues();
assert iter.next() == ?0;
assert iter.next() == ?1;
assert iter.next() == ?2;
// ...
```
