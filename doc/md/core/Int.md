# core/Int
Signed integer numbers with infinite precision (also called big integers).

Most operations on integer numbers (e.g. addition) are available as built-in operators (e.g. `-1 + 1`).
This module provides equivalent functions and `Text` conversion.

Import from the core package to use this module.
```motoko name=import
import Int "mo:core/Int";
```

## Type `Int`
``` motoko no-repl
type Int = Prim.Types.Int
```

Infinite precision signed integers.

## Function `abs`
``` motoko no-repl
func abs(x : Int) : Nat
```

Returns the absolute value of `x`.

Example:
```motoko include=import
assert Int.abs(-12) == 12;
```

## Function `toText`
``` motoko no-repl
func toText(self : Int) : Text
```

Converts an integer number to its textual representation. Textual
representation _do not_ contain underscores to represent commas.

Example:
```motoko include=import
assert Int.toText(-1234) == "-1234";
```

## Function `fromText`
``` motoko no-repl
func fromText(text : Text) : ?Int
```

Creates a integer from its textual representation. Returns `null`
if the input is not a valid integer.

The textual representation _must not_ contain underscores but may
begin with a '+' or '-' character.

Example:
```motoko include=import
assert Int.fromText("-1234") == ?-1234;
```

## Function `toInt`
``` motoko no-repl
func toInt(self : Text) : ?Int
```

Creates a integer from its textual representation. Returns `null`
if the input is not a valid integer.

This functions is meant to be used with contextual-dot notation.

Example:
```motoko include=import
assert "-1234".toInt() == ?-1234;
```

## Function `toNat`
``` motoko no-repl
func toNat(self : Int) : Nat
```

Converts an integer to a natural number. Traps if the integer is negative.

Example:
```motoko include=import
import Debug "mo:core/Debug";
assert Int.toNat(1234 : Int) == (1234 : Nat);
```

## Function `fromNat`
``` motoko no-repl
func fromNat(nat : Nat) : Int
```

Converts a natural number to an integer.

Example:
```motoko include=import
assert Int.fromNat(1234 : Nat) == (1234 : Int);
```

## Function `toFloat`
``` motoko no-repl
func toFloat(self : Int) : Float
```

Conversion to Float. May result in `Inf`.

Note: The floating point number may be imprecise for large or small Int values.
Returns `inf` if the integer is greater than the maximum floating point number.
Returns `-inf` if the integer is less than the minimum floating point number.

Example:
```motoko include=import
assert Int.toFloat(-123) == -123.0;
```

## Function `toInt8`
``` motoko no-repl
func toInt8(self : Int) : Int8
```

Converts a signed integer with infinite precision to an 8-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko include=import
assert Int.toInt8(123) == (123 : Int8);
```

## Function `toInt16`
``` motoko no-repl
func toInt16(self : Int) : Int16
```

Converts a signed integer with infinite precision to a 16-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko include=import
assert Int.toInt16(12_345) == (12_345 : Int16);
```

## Function `toInt32`
``` motoko no-repl
func toInt32(self : Int) : Int32
```

Converts a signed integer with infinite precision to a 32-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko include=import
assert Int.toInt32(123_456) == (123_456 : Int32);
```

## Function `toInt64`
``` motoko no-repl
func toInt64(self : Int) : Int64
```

Converts a signed integer with infinite precision to a 64-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko include=import
assert Int.toInt64(123_456_789) == (123_456_789 : Int64);
```

## Function `fromInt8`
``` motoko no-repl
func fromInt8(x : Int8) : Int
```

Converts an 8-bit signed integer to a signed integer with infinite precision.

Example:
```motoko include=import
assert Int.fromInt8(123 : Int8) == 123;
```

## Function `fromInt16`
``` motoko no-repl
func fromInt16(x : Int16) : Int
```

Converts a 16-bit signed integer to a signed integer with infinite precision.

Example:
```motoko include=import
assert Int.fromInt16(12_345 : Int16) == 12_345;
```

## Function `fromInt32`
``` motoko no-repl
func fromInt32(x : Int32) : Int
```

Converts a 32-bit signed integer to a signed integer with infinite precision.

Example:
```motoko include=import
assert Int.fromInt32(123_456 : Int32) == 123_456;
```

## Function `fromInt64`
``` motoko no-repl
func fromInt64(x : Int64) : Int
```

Converts a 64-bit signed integer to a signed integer with infinite precision.

Example:
```motoko include=import
assert Int.fromInt64(123_456_789 : Int64) == 123_456_789;
```

## Function `min`
``` motoko no-repl
func min(x : Int, y : Int) : Int
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
assert Int.min(2, -3) == -3;
```

## Function `max`
``` motoko no-repl
func max(x : Int, y : Int) : Int
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
assert Int.max(2, -3) == 2;
```

## Function `equal`
``` motoko no-repl
func equal(x : Int, y : Int) : Bool
```

Equality function for Int types.
This is equivalent to `x == y`.

Example:
```motoko include=import
assert Int.equal(-1, -1);
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
let a : Int = 1;
let b : Int = -1;
assert not Int.equal(a, b);
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int, y : Int) : Bool
```

Inequality function for Int types.
This is equivalent to `x != y`.

Example:
```motoko include=import
assert Int.notEqual(-1, -2);
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Int, y : Int) : Bool
```

"Less than" function for Int types.
This is equivalent to `x < y`.

Example:
```motoko include=import
assert Int.less(-2, 1);
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Int, y : Int) : Bool
```

"Less than or equal" function for Int types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
assert Int.lessOrEqual(-2, 1);
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Int, y : Int) : Bool
```

"Greater than" function for Int types.
This is equivalent to `x > y`.

Example:
```motoko include=import
assert Int.greater(1, -2);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Int, y : Int) : Bool
```

"Greater than or equal" function for Int types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
assert Int.greaterOrEqual(1, -2);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Int, y : Int) : Order.Order
```

General-purpose comparison function for `Int`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
assert Int.compare(-3, 2) == #less;
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.sort([1, -2, -3], Int.compare) == [-3, -2, 1];
```

## Function `neg`
``` motoko no-repl
func neg(x : Int) : Int
```

Returns the negation of `x`, `-x` .

Example:
```motoko include=import
assert Int.neg(123) == -123;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

## Function `add`
``` motoko no-repl
func add(x : Int, y : Int) : Int
```

Returns the sum of `x` and `y`, `x + y`.

No overflow since `Int` has infinite precision.

Example:
```motoko include=import
assert Int.add(1, -2) == -1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([1, -2, -3], 0, Int.add) == -4;
```

## Function `sub`
``` motoko no-repl
func sub(x : Int, y : Int) : Int
```

Returns the difference of `x` and `y`, `x - y`.

No overflow since `Int` has infinite precision.

Example:
```motoko include=import
assert Int.sub(1, 2) == -1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([1, -2, -3], 0, Int.sub) == 4;
```

## Function `mul`
``` motoko no-repl
func mul(x : Int, y : Int) : Int
```

Returns the product of `x` and `y`, `x * y`.

No overflow since `Int` has infinite precision.

Example:
```motoko include=import
assert Int.mul(-2, 3) == -6;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft([1, -2, -3], 1, Int.mul) == 6;
```

## Function `div`
``` motoko no-repl
func div(x : Int, y : Int) : Int
```

Returns the signed integer division of `x` by `y`,  `x / y`.
Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.

Traps when `y` is zero.

Example:
```motoko include=import
assert Int.div(6, -2) == -3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Int, y : Int) : Int
```

Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
which is defined as `x - x / y * y`.

Traps when `y` is zero.

Example:
```motoko include=import
assert Int.rem(6, -4) == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Int, y : Int) : Int
```

Returns `x` to the power of `y`, `x ** y`.

Traps when `y` is negative or `y > 2 ** 32 - 1`.
No overflow since `Int` has infinite precision.

Example:
```motoko include=import
assert Int.pow(-2, 3) == -8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `range`
``` motoko no-repl
func range(fromInclusive : Int, toExclusive : Int) : Iter.Iter<Int>
```

Returns an iterator over the integers from the first to second argument with an exclusive upper bound.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Int.range(1, 4);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Int.range(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeBy`
``` motoko no-repl
func rangeBy(fromInclusive : Int, toExclusive : Int, step : Int) : Iter.Iter<Int>
```

Returns an iterator over `Int` values from the first to second argument with an exclusive upper bound,
incrementing by the specified step size.
```motoko include=import
import Iter "mo:core/Iter";

// Positive step
let iter1 = Int.rangeBy(1, 7, 2);
assert iter1.next() == ?1;
assert iter1.next() == ?3;
assert iter1.next() == ?5;
assert iter1.next() == null;

// Negative step
let iter2 = Int.rangeBy(7, 1, -2);
assert iter2.next() == ?7;
assert iter2.next() == ?5;
assert iter2.next() == ?3;
assert iter2.next() == null;
```

If `step` is 0 or if the iteration would not progress towards the bound, returns an empty iterator.

## Function `rangeInclusive`
``` motoko no-repl
func rangeInclusive(from : Int, to : Int) : Iter.Iter<Int>
```

Returns an iterator over the integers from the first to second argument, inclusive.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Int.rangeInclusive(1, 3);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Int.rangeInclusive(3, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeByInclusive`
``` motoko no-repl
func rangeByInclusive(from : Int, to : Int, step : Int) : Iter.Iter<Int>
```

Returns an iterator over the integers from the first to second argument, inclusive,
incrementing by the specified step size.
```motoko include=import
import Iter "mo:core/Iter";

// Positive step
let iter1 = Int.rangeByInclusive(1, 7, 2);
assert iter1.next() == ?1;
assert iter1.next() == ?3;
assert iter1.next() == ?5;
assert iter1.next() == ?7;
assert iter1.next() == null;

// Negative step
let iter2 = Int.rangeByInclusive(7, 1, -2);
assert iter2.next() == ?7;
assert iter2.next() == ?5;
assert iter2.next() == ?3;
assert iter2.next() == ?1;
assert iter2.next() == null;
```

If `from == to`, return an iterator which only returns that value.

Otherwise, if `step` is 0 or if the iteration would not progress towards the bound, returns an empty iterator.
