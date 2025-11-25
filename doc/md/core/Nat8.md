# core/Nat8
Utility functions on 8-bit unsigned integers.

Note that most operations are available as built-in operators (e.g. `1 + 1`).

Import from the core package to use this module.
```motoko name=import
import Nat8 "mo:core/Nat8";
```

## Type `Nat8`
``` motoko no-repl
type Nat8 = Prim.Types.Nat8
```

8-bit natural numbers.

## Value `maxValue`
``` motoko no-repl
let maxValue : Nat8
```

Maximum 8-bit natural number. `2 ** 8 - 1`.

Example:
```motoko include=import
assert Nat8.maxValue == (255 : Nat8);
```

## Function `toNat`
``` motoko no-repl
func toNat(_ : Nat8) : Nat
```

Converts an 8-bit unsigned integer to an unsigned integer with infinite precision.

Example:
```motoko include=import
assert Nat8.toNat(123) == (123 : Nat);
```

## Function `fromNat`
``` motoko no-repl
func fromNat(_ : Nat) : Nat8
```

Converts an unsigned integer with infinite precision to an 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat8.fromNat(123) == (123 : Nat8);
```

## Function `fromNat16`
``` motoko no-repl
func fromNat16(_ : Nat16) : Nat8
```

Converts a 16-bit unsigned integer to a 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat8.fromNat16(123) == (123 : Nat8);
```

## Function `toNat16`
``` motoko no-repl
func toNat16(_ : Nat8) : Nat16
```

Converts an 8-bit unsigned integer to a 16-bit unsigned integer.

Example:
```motoko include=import
assert Nat8.toNat16(123) == (123 : Nat16);
```

## Function `fromIntWrap`
``` motoko no-repl
func fromIntWrap(_ : Int) : Nat8
```

Converts a signed integer with infinite precision to an 8-bit unsigned integer.

Wraps on overflow/underflow.

Example:
```motoko include=import
assert Nat8.fromIntWrap(123) == (123 : Nat8);
```

## Function `toText`
``` motoko no-repl
func toText(x : Nat8) : Text
```

Converts `x` to its textual representation.

Example:
```motoko include=import
assert Nat8.toText(123) == ("123" : Text);
```

## Function `min`
``` motoko no-repl
func min(x : Nat8, y : Nat8) : Nat8
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
assert Nat8.min(123, 200) == (123 : Nat8);
```

## Function `max`
``` motoko no-repl
func max(x : Nat8, y : Nat8) : Nat8
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
assert Nat8.max(123, 200) == (200 : Nat8);
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat8, y : Nat8) : Bool
```

Equality function for Nat8 types.
This is equivalent to `x == y`.

Example:
```motoko include=import
assert Nat8.equal(1, 1);
assert (1 : Nat8) == (1 : Nat8);
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
let a : Nat8 = 111;
let b : Nat8 = 222;
assert not Nat8.equal(a, b);
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat8, y : Nat8) : Bool
```

Inequality function for Nat8 types.
This is equivalent to `x != y`.

Example:
```motoko include=import
assert Nat8.notEqual(1, 2);
assert (1 : Nat8) != (2 : Nat8);
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Nat8, y : Nat8) : Bool
```

"Less than" function for Nat8 types.
This is equivalent to `x < y`.

Example:
```motoko include=import
assert Nat8.less(1, 2);
assert (1 : Nat8) < (2 : Nat8);
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat8, y : Nat8) : Bool
```

"Less than or equal" function for Nat8 types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
assert Nat8.lessOrEqual(1, 2);
assert 1 <= 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Nat8, y : Nat8) : Bool
```

"Greater than" function for Nat8 types.
This is equivalent to `x > y`.

Example:
```motoko include=import
assert Nat8.greater(2, 1);
assert (2 : Nat8) > (1 : Nat8);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat8, y : Nat8) : Bool
```

"Greater than or equal" function for Nat8 types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
assert Nat8.greaterOrEqual(2, 1);
assert (2 : Nat8) >= (1 : Nat8);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat8, y : Nat8) : Order.Order
```

General purpose comparison function for `Nat8`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
assert Nat8.compare(2, 3) == #less;
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.sort([2, 3, 1] : [Nat8], Nat8.compare) == [1, 2, 3];
```

## Function `add`
``` motoko no-repl
func add(x : Nat8, y : Nat8) : Nat8
```

Returns the sum of `x` and `y`, `x + y`.
Traps on overflow.

Example:
```motoko include=import
assert Nat8.add(1, 2) == 3;
assert (1 : Nat8) + (2 : Nat8) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat8, Nat8>([2, 3, 1], 0, Nat8.add) == 6;
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat8, y : Nat8) : Nat8
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

Example:
```motoko include=import
assert Nat8.sub(2, 1) == 1;
assert (2 : Nat8) - (1 : Nat8) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat8, Nat8>([2, 3, 1], 20, Nat8.sub) == 14;
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat8, y : Nat8) : Nat8
```

Returns the product of `x` and `y`, `x * y`.
Traps on overflow.

Example:
```motoko include=import
assert Nat8.mul(2, 3) == 6;
assert (2 : Nat8) * (3 : Nat8) == 6;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat8, Nat8>([2, 3, 1], 1, Nat8.mul) == 6;
```

## Function `div`
``` motoko no-repl
func div(x : Nat8, y : Nat8) : Nat8
```

Returns the quotient of `x` divided by `y`, `x / y`.
Traps when `y` is zero.

Example:
```motoko include=import
assert Nat8.div(6, 2) == 3;
assert (6 : Nat8) / (2 : Nat8) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Nat8, y : Nat8) : Nat8
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

Example:
```motoko include=import
assert Nat8.rem(6, 4) == 2;
assert (6 : Nat8) % (4 : Nat8) == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Nat8, y : Nat8) : Nat8
```

Returns `x` to the power of `y`, `x ** y`.
Traps on overflow.

Example:
```motoko include=import
assert Nat8.pow(2, 3) == 8;
assert (2 : Nat8) ** (3 : Nat8) == 8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Nat8) : Nat8
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko include=import
assert Nat8.bitnot(0) == 255;
assert ^(0 : Nat8) == 255;
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitand`
``` motoko no-repl
func bitand(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise and of `x` and `y`, `x & y`.

Example:
```motoko include=import
assert Nat8.bitand(3, 2) == 2;
assert (3 : Nat8) & (2 : Nat8) == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `&` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `&`
as a function value at the moment.

## Function `bitor`
``` motoko no-repl
func bitor(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise or of `x` and `y`, `x | y`.

Example:
```motoko include=import
assert Nat8.bitor(3, 2) == 3;
assert (3 : Nat8) | (2 : Nat8) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `|` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `|`
as a function value at the moment.

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

Example:
```motoko include=import
assert Nat8.bitxor(3, 2) == 1;
assert (3 : Nat8) ^ (2 : Nat8) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

Example:
```motoko include=import
assert Nat8.bitshiftLeft(1, 2) == 4;
assert (1 : Nat8) << (2 : Nat8) == 4;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<`
as a function value at the moment.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

Example:
```motoko include=import
assert Nat8.bitshiftRight(4, 2) == 1;
assert (4 : Nat8) >> (2 : Nat8) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>>`
as a function value at the moment.

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

Example:
```motoko include=import
assert Nat8.bitrotLeft(128, 1) == 1;
assert (128 : Nat8) <<> (1 : Nat8) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<>`
as a function value at the moment.

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Nat8, y : Nat8) : Nat8
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

Example:
```motoko include=import
assert Nat8.bitrotRight(1, 1) == 128;
assert (1 : Nat8) <>> (1 : Nat8) == 128;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<>>`
as a function value at the moment.

## Function `bittest`
``` motoko no-repl
func bittest(x : Nat8, p : Nat) : Bool
```

Returns the value of bit `p mod 8` in `x`, `(x & 2^(p mod 8)) == 2^(p mod 8)`.
This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.

Example:
```motoko include=import
assert Nat8.bittest(5, 2);
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat8, p : Nat) : Nat8
```

Returns the value of setting bit `p mod 8` in `x` to `1`.

Example:
```motoko include=import
assert Nat8.bitset(5, 1) == 7;
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat8, p : Nat) : Nat8
```

Returns the value of clearing bit `p mod 8` in `x` to `0`.

Example:
```motoko include=import
assert Nat8.bitclear(5, 2) == 1;
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat8, p : Nat) : Nat8
```

Returns the value of flipping bit `p mod 8` in `x`.

Example:
```motoko include=import
assert Nat8.bitflip(5, 2) == 1;
```

## Function `bitcountNonZero`
``` motoko no-repl
func bitcountNonZero(x : Nat8) : Nat8
```

Returns the count of non-zero bits in `x`.

Example:
```motoko include=import
assert Nat8.bitcountNonZero(5) == 2;
```

## Function `bitcountLeadingZero`
``` motoko no-repl
func bitcountLeadingZero(x : Nat8) : Nat8
```

Returns the count of leading zero bits in `x`.

Example:
```motoko include=import
assert Nat8.bitcountLeadingZero(5) == 5;
```

## Function `bitcountTrailingZero`
``` motoko no-repl
func bitcountTrailingZero(x : Nat8) : Nat8
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko include=import
assert Nat8.bitcountTrailingZero(6) == 1;
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat8, y : Nat8) : Nat8
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat8.addWrap(230, 26) == 0;
assert (230 : Nat8) +% (26 : Nat8) == 0;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+%`
as a function value at the moment.

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Nat8, y : Nat8) : Nat8
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

Example:
```motoko include=import
assert Nat8.subWrap(0, 1) == 255;
assert (0 : Nat8) -% (1 : Nat8) == 255;
```
Note: The reason why this function is defined in this library (in addition
to the existing `-%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-%`
as a function value at the moment.

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Nat8, y : Nat8) : Nat8
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat8.mulWrap(230, 26) == 92;
assert (230 : Nat8) *% (26 : Nat8) == 92;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*%`
as a function value at the moment.

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Nat8, y : Nat8) : Nat8
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat8.powWrap(2, 8) == 0;
assert (2 : Nat8) **% (8 : Nat8) == 0;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**%`
as a function value at the moment.

## Function `range`
``` motoko no-repl
func range(fromInclusive : Nat8, toExclusive : Nat8) : Iter.Iter<Nat8>
```

Returns an iterator over `Nat8` values from the first to second argument with an exclusive upper bound.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat8.range(1, 4);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat8.range(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeInclusive`
``` motoko no-repl
func rangeInclusive(from : Nat8, to : Nat8) : Iter.Iter<Nat8>
```

Returns an iterator over `Nat8` values from the first to second argument, inclusive.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat8.rangeInclusive(1, 3);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat8.rangeInclusive(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `allValues`
``` motoko no-repl
func allValues() : Iter.Iter<Nat8>
```

Returns an iterator over all Nat8 values, from 0 to maxValue.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat8.allValues();
assert iter.next() == ?0;
assert iter.next() == ?1;
assert iter.next() == ?2;
// ...
```
