# core/Nat32
Utility functions on 32-bit unsigned integers.

Note that most operations are available as built-in operators (e.g. `1 + 1`).

Import from the core package to use this module.
```motoko name=import
import Nat32 "mo:core/Nat32";
```

## Type `Nat32`
``` motoko no-repl
type Nat32 = Prim.Types.Nat32
```

32-bit natural numbers.

## Value `maxValue`
``` motoko no-repl
let maxValue : Nat32
```

Maximum 32-bit natural number. `2 ** 32 - 1`.

Example:
```motoko include=import
assert Nat32.maxValue == (4294967295 : Nat32);
```

## Function `toNat`
``` motoko no-repl
func toNat(self : Nat32) : Nat
```

Converts a 32-bit unsigned integer to an unsigned integer with infinite precision.

Example:
```motoko include=import
assert Nat32.toNat(123) == (123 : Nat);
```

## Function `fromNat`
``` motoko no-repl
func fromNat(_ : Nat) : Nat32
```

Converts an unsigned integer with infinite precision to a 32-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat32.fromNat(123) == (123 : Nat32);
```

## Function `toNat8`
``` motoko no-repl
func toNat8(self : Nat32) : Nat8
```

Converts a 32-bit unsigned integer to an 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat32.toNat8(123) == (123 : Nat8);
```

## Function `fromNat8`
``` motoko no-repl
func fromNat8(x : Nat8) : Nat32
```

Converts an 8-bit unsigned integer to a 32-bit unsigned integer.

Example:
```motoko include=import
assert Nat32.fromNat8(123) == (123 : Nat32);
```
@deprecated M0235

## Function `fromNat16`
``` motoko no-repl
func fromNat16(x : Nat16) : Nat32
```

Converts a 16-bit unsigned integer to a 32-bit unsigned integer.

Example:
```motoko include=import
assert Nat32.fromNat16(123) == (123 : Nat32);
```
@deprecated M0235

## Function `toNat16`
``` motoko no-repl
func toNat16(self : Nat32) : Nat16
```

Converts a 32-bit unsigned integer to a 16-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat32.toNat16(123) == (123 : Nat16);
```

## Function `fromNat64`
``` motoko no-repl
func fromNat64(x : Nat64) : Nat32
```

Converts a 64-bit unsigned integer to a 32-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
assert Nat32.fromNat64(123) == (123 : Nat32);
```
@deprecated M0235

## Function `toNat64`
``` motoko no-repl
func toNat64(self : Nat32) : Nat64
```

Converts a 32-bit unsigned integer to a 64-bit unsigned integer.

Example:
```motoko include=import
assert Nat32.toNat64(123) == (123 : Nat64);
```

## Function `fromIntWrap`
``` motoko no-repl
func fromIntWrap(_ : Int) : Nat32
```

Converts a signed integer with infinite precision to a 32-bit unsigned integer.

Traps on overflow/underflow.

Example:
```motoko include=import
assert Nat32.fromIntWrap(123) == (123 : Nat32);
```

## Function `toChar`
``` motoko no-repl
func toChar(self : Nat32) : Char
```

Convert a Nat32 `char` to a Char in its Unicode representation.

Example:
```motoko include=import
let unicode = Nat32.toChar(65);
assert unicode == 'A';
```

## Function `toText`
``` motoko no-repl
func toText(self : Nat32) : Text
```

Converts `x` to its textual representation. Textual representation _do not_
contain underscores to represent commas.

Example:
```motoko include=import
assert Nat32.toText(1234) == ("1234" : Text);
```

## Function `min`
``` motoko no-repl
func min(x : Nat32, y : Nat32) : Nat32
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
assert Nat32.min(123, 456) == (123 : Nat32);
```

## Function `max`
``` motoko no-repl
func max(x : Nat32, y : Nat32) : Nat32
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
assert Nat32.max(123, 456) == (456 : Nat32);
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat32, y : Nat32) : Bool
```

Equality function for Nat32 types.
This is equivalent to `x == y`.

Example:
```motoko include=import
assert Nat32.equal(1, 1);
assert (1 : Nat32) == (1 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
let a : Nat32 = 111;
let b : Nat32 = 222;
assert not Nat32.equal(a, b);
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat32, y : Nat32) : Bool
```

Inequality function for Nat32 types.
This is equivalent to `x != y`.

Example:
```motoko include=import
assert Nat32.notEqual(1, 2);
assert (1 : Nat32) != (2 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Nat32, y : Nat32) : Bool
```

"Less than" function for Nat32 types.
This is equivalent to `x < y`.

Example:
```motoko include=import
assert Nat32.less(1, 2);
assert (1 : Nat32) < (2 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat32, y : Nat32) : Bool
```

"Less than or equal" function for Nat32 types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
assert Nat32.lessOrEqual(1, 2);
assert (1 : Nat32) <= (2 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Nat32, y : Nat32) : Bool
```

"Greater than" function for Nat32 types.
This is equivalent to `x > y`.

Example:
```motoko include=import
assert Nat32.greater(2, 1);
assert (2 : Nat32) > (1 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat32, y : Nat32) : Bool
```

"Greater than or equal" function for Nat32 types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
assert Nat32.greaterOrEqual(2, 1);
assert (2 : Nat32) >= (1 : Nat32);
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat32, y : Nat32) : Order.Order
```

General purpose comparison function for `Nat32`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
assert Nat32.compare(2, 3) == #less;
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.sort([2, 3, 1] : [Nat32], Nat32.compare) == [1, 2, 3];
```

## Function `add`
``` motoko no-repl
func add(x : Nat32, y : Nat32) : Nat32
```

Returns the sum of `x` and `y`, `x + y`.
Traps on overflow.

Example:
```motoko include=import
assert Nat32.add(1, 2) == 3;
assert (1 : Nat32) + (2 : Nat32) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat32, Nat32>([2, 3, 1], 0, Nat32.add) == 6;
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat32, y : Nat32) : Nat32
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

Example:
```motoko include=import
assert Nat32.sub(2, 1) == 1;
assert (2 : Nat32) - (1 : Nat32) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat32, Nat32>([2, 3, 1], 20, Nat32.sub) == 14;
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat32, y : Nat32) : Nat32
```

Returns the product of `x` and `y`, `x * y`.
Traps on overflow.

Example:
```motoko include=import
assert Nat32.mul(2, 3) == 6;
assert (2 : Nat32) * (3 : Nat32) == 6;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:core/Array";
assert Array.foldLeft<Nat32, Nat32>([2, 3, 1], 1, Nat32.mul) == 6;
```

## Function `div`
``` motoko no-repl
func div(x : Nat32, y : Nat32) : Nat32
```

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

Example:
```motoko include=import
assert Nat32.div(6, 2) == 3;
assert (6 : Nat32) / (2 : Nat32) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Nat32, y : Nat32) : Nat32
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

Example:
```motoko include=import
assert Nat32.rem(6, 4) == 2;
assert (6 : Nat32) % (4 : Nat32) == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Nat32, y : Nat32) : Nat32
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

Example:
```motoko include=import
assert Nat32.pow(2, 3) == 8;
assert (2 : Nat32) ** (3 : Nat32) == 8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Nat32) : Nat32
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko include=import
assert Nat32.bitnot(0) == 4294967295;
assert ^(0 : Nat32) == 4294967295;
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitand`
``` motoko no-repl
func bitand(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise and of `x` and `y`, `x & y`.

Example:
```motoko include=import
assert Nat32.bitand(1, 3) == 1;
assert (1 : Nat32) & (3 : Nat32) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `&` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `&`
as a function value at the moment.

## Function `bitor`
``` motoko no-repl
func bitor(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise or of `x` and `y`, `x | y`.

Example:
```motoko include=import
assert Nat32.bitor(1, 3) == 3;
assert (1 : Nat32) | (3 : Nat32) == 3;
```

Note: The reason why this function is defined in this library (in addition
to the existing `|` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `|`
as a function value at the moment.

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

Example:
```motoko include=import
assert Nat32.bitxor(1, 3) == 2;
assert (1 : Nat32) ^ (3 : Nat32) == 2;
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

Example:
```motoko include=import
assert Nat32.bitshiftLeft(1, 3) == 8;
assert (1 : Nat32) << (3 : Nat32) == 8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<`
as a function value at the moment.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

Example:
```motoko include=import
assert Nat32.bitshiftRight(8, 3) == 1;
assert (8 : Nat32) >> (3 : Nat32) == 1;
```

Note: The reason why this function is defined in this library (in addition
to the existing `>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>>`
as a function value at the moment.

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

Example:
```motoko include=import
assert Nat32.bitrotLeft(1, 3) == 8;
assert (1 : Nat32) <<> (3 : Nat32) == 8;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<>`
as a function value at the moment.

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Nat32, y : Nat32) : Nat32
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

Example:
```motoko include=import
assert Nat32.bitrotRight(1, 1) == 2147483648;
assert (1 : Nat32) <>> (1 : Nat32) == 2147483648;
```

Note: The reason why this function is defined in this library (in addition
to the existing `<>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<>>`
as a function value at the moment.

## Function `bittest`
``` motoko no-repl
func bittest(x : Nat32, p : Nat) : Bool
```

Returns the value of bit `p mod 32` in `x`, `(x & 2^(p mod 32)) == 2^(p mod 32)`.
This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.

Example:
```motoko include=import
assert Nat32.bittest(5, 2);
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat32, p : Nat) : Nat32
```

Returns the value of setting bit `p mod 32` in `x` to `1`.

Example:
```motoko include=import
assert Nat32.bitset(5, 1) == 7;
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat32, p : Nat) : Nat32
```

Returns the value of clearing bit `p mod 32` in `x` to `0`.

Example:
```motoko include=import
assert Nat32.bitclear(5, 2) == 1;
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat32, p : Nat) : Nat32
```

Returns the value of flipping bit `p mod 32` in `x`.

Example:
```motoko include=import
assert Nat32.bitflip(5, 2) == 1;
```

## Function `bitcountNonZero`
``` motoko no-repl
func bitcountNonZero(x : Nat32) : Nat32
```

Returns the count of non-zero bits in `x`.

Example:
```motoko include=import
assert Nat32.bitcountNonZero(5) == 2;
```

## Function `bitcountLeadingZero`
``` motoko no-repl
func bitcountLeadingZero(x : Nat32) : Nat32
```

Returns the count of leading zero bits in `x`.

Example:
```motoko include=import
assert Nat32.bitcountLeadingZero(5) == 29;
```

## Function `bitcountTrailingZero`
``` motoko no-repl
func bitcountTrailingZero(x : Nat32) : Nat32
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko include=import
assert Nat32.bitcountTrailingZero(16) == 4;
```

## Function `explode`
``` motoko no-repl
func explode(x : Nat32) : (msb : Nat8, Nat8, Nat8, lsb : Nat8)
```

Returns the upper (i.e. most significant), lower (least significant)
and in-between bytes of `x`.

Example:
```motoko include=import
assert Nat32.explode 0xaa885511 == (170, 136, 85, 17);
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat32, y : Nat32) : Nat32
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat32.addWrap(4294967295, 1) == 0;
assert (4294967295 : Nat32) +% (1 : Nat32) == 0;
```

Note: The reason why this function is defined in this library (in addition
to the existing `+%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+%`
as a function value at the moment.

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Nat32, y : Nat32) : Nat32
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

Example:
```motoko include=import
assert Nat32.subWrap(0, 1) == 4294967295;
assert (0 : Nat32) -% (1 : Nat32) == 4294967295;
```

Note: The reason why this function is defined in this library (in addition
to the existing `-%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-%`
as a function value at the moment.

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Nat32, y : Nat32) : Nat32
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat32.mulWrap(2147483648, 2) == 0;
assert (2147483648 : Nat32) *% (2 : Nat32) == 0;
```

Note: The reason why this function is defined in this library (in addition
to the existing `*%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*%`
as a function value at the moment.

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Nat32, y : Nat32) : Nat32
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.

Example:
```motoko include=import
assert Nat32.powWrap(2, 32) == 0;
assert (2 : Nat32) **% (32 : Nat32) == 0;
```

Note: The reason why this function is defined in this library (in addition
to the existing `**%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**%`
as a function value at the moment.

## Function `range`
``` motoko no-repl
func range(fromInclusive : Nat32, toExclusive : Nat32) : Iter.Iter<Nat32>
```

Returns an iterator over `Nat32` values from the first to second argument with an exclusive upper bound.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat32.range(1, 4);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat32.range(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `rangeInclusive`
``` motoko no-repl
func rangeInclusive(from : Nat32, to : Nat32) : Iter.Iter<Nat32>
```

Returns an iterator over `Nat32` values from the first to second argument, inclusive.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat32.rangeInclusive(1, 3);
assert iter.next() == ?1;
assert iter.next() == ?2;
assert iter.next() == ?3;
assert iter.next() == null;
```

If the first argument is greater than the second argument, the function returns an empty iterator.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat32.rangeInclusive(4, 1);
assert iter.next() == null; // empty iterator
```

## Function `allValues`
``` motoko no-repl
func allValues() : Iter.Iter<Nat32>
```

Returns an iterator over all Nat32 values, from 0 to maxValue.
```motoko include=import
import Iter "mo:core/Iter";

let iter = Nat32.allValues();
assert iter.next() == ?0;
assert iter.next() == ?1;
assert iter.next() == ?2;
// ...
```
