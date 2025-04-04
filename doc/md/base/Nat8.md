# Nat8
Provides utility functions on 8-bit unsigned integers.

Note that most operations are available as built-in operators (e.g. `1 + 1`).

Import from the base library to use this module.
```motoko name=import
import Nat8 "mo:base/Nat8";
```

## Type `Nat8`
``` motoko no-repl
type Nat8 = Prim.Types.Nat8
```

8-bit natural numbers.

## Value `maximumValue`
``` motoko no-repl
let maximumValue : Nat8
```

Maximum 8-bit natural number. `2 ** 8 - 1`.

Example:
```motoko include=import
Nat8.maximumValue; // => 255 : Nat8
```

## Value `toNat`
``` motoko no-repl
let toNat : Nat8 -> Nat
```

Converts an 8-bit unsigned integer to an unsigned integer with infinite precision.

Example:
```motoko include=import
Nat8.toNat(123); // => 123 : Nat
```

## Value `fromNat`
``` motoko no-repl
let fromNat : Nat -> Nat8
```

Converts an unsigned integer with infinite precision to an 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat8.fromNat(123); // => 123 : Nat8
```

## Value `fromNat16`
``` motoko no-repl
let fromNat16 : Nat16 -> Nat8
```

Converts a 16-bit unsigned integer to a 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat8.fromNat16(123); // => 123 : Nat8
```

## Value `toNat16`
``` motoko no-repl
let toNat16 : Nat8 -> Nat16
```

Converts an 8-bit unsigned integer to a 16-bit unsigned integer.

Example:
```motoko include=import
Nat8.toNat16(123); // => 123 : Nat16
```

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Nat8
```

Converts a signed integer with infinite precision to an 8-bit unsigned integer.

Wraps on overflow/underflow.

Example:
```motoko include=import
Nat8.fromIntWrap(123); // => 123 : Nat8
```

## Function `toText`
``` motoko no-repl
func toText(x : Nat8) : Text
```

Converts `x` to its textual representation.

Example:
```motoko include=import
Nat8.toText(123); // => "123" : Text
```

## Function `min`
``` motoko no-repl
func min(x : Nat8, y : Nat8) : Nat8
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
Nat8.min(123, 200); // => 123 : Nat8
```

## Function `max`
``` motoko no-repl
func max(x : Nat8, y : Nat8) : Nat8
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
Nat8.max(123, 200); // => 200 : Nat8
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat8, y : Nat8) : Bool
```

Equality function for Nat8 types.
This is equivalent to `x == y`.

Example:
```motoko include=import
ignore Nat8.equal(1, 1); // => true
(1 : Nat8) == (1 : Nat8) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
import Buffer "mo:base/Buffer";

let buffer1 = Buffer.Buffer<Nat8>(3);
let buffer2 = Buffer.Buffer<Nat8>(3);
Buffer.equal(buffer1, buffer2, Nat8.equal) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat8, y : Nat8) : Bool
```

Inequality function for Nat8 types.
This is equivalent to `x != y`.

Example:
```motoko include=import
ignore Nat8.notEqual(1, 2); // => true
(1 : Nat8) != (2 : Nat8) // => true
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
ignore Nat8.less(1, 2); // => true
(1 : Nat8) < (2 : Nat8) // => true
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
ignore Nat.lessOrEqual(1, 2); // => true
1 <= 2 // => true
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
ignore Nat8.greater(2, 1); // => true
(2 : Nat8) > (1 : Nat8) // => true
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
ignore Nat8.greaterOrEqual(2, 1); // => true
(2 : Nat8) >= (1 : Nat8) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat8, y : Nat8) : {#less; #equal; #greater}
```

General purpose comparison function for `Nat8`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
Nat8.compare(2, 3) // => #less
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.sort([2, 3, 1] : [Nat8], Nat8.compare) // => [1, 2, 3]
```

## Function `add`
``` motoko no-repl
func add(x : Nat8, y : Nat8) : Nat8
```

Returns the sum of `x` and `y`, `x + y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat8.add(1, 2); // => 3
(1 : Nat8) + (2 : Nat8) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat8, Nat8>([2, 3, 1], 0, Nat8.add) // => 6
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat8, y : Nat8) : Nat8
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

Example:
```motoko include=import
ignore Nat8.sub(2, 1); // => 1
(2 : Nat8) - (1 : Nat8) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat8, Nat8>([2, 3, 1], 20, Nat8.sub) // => 14
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat8, y : Nat8) : Nat8
```

Returns the product of `x` and `y`, `x * y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat8.mul(2, 3); // => 6
(2 : Nat8) * (3 : Nat8) // => 6
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat8, Nat8>([2, 3, 1], 1, Nat8.mul) // => 6
```

## Function `div`
``` motoko no-repl
func div(x : Nat8, y : Nat8) : Nat8
```

Returns the quotient of `x` divided by `y`, `x / y`.
Traps when `y` is zero.

Example:
```motoko include=import
ignore Nat8.div(6, 2); // => 3
(6 : Nat8) / (2 : Nat8) // => 3
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
ignore Nat8.rem(6, 4); // => 2
(6 : Nat8) % (4 : Nat8) // => 2
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
ignore Nat8.pow(2, 3); // => 8
(2 : Nat8) ** (3 : Nat8) // => 8
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
ignore Nat8.bitnot(0); // => 255
^(0 : Nat8) // => 255
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
ignore Nat8.bitand(3, 2); // => 2
(3 : Nat8) & (2 : Nat8) // => 2
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
ignore Nat8.bitor(3, 2); // => 3
(3 : Nat8) | (2 : Nat8) // => 3
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
ignore Nat8.bitxor(3, 2); // => 1
(3 : Nat8) ^ (2 : Nat8) // => 1
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
ignore Nat8.bitshiftLeft(1, 2); // => 4
(1 : Nat8) << (2 : Nat8) // => 4
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
ignore Nat8.bitshiftRight(4, 2); // => 1
(4 : Nat8) >> (2 : Nat8) // => 1
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
ignore Nat8.bitrotLeft(128, 1); // => 1
(128 : Nat8) <<> (1 : Nat8) // => 1
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
ignore Nat8.bitrotRight(1, 1); // => 128
(1 : Nat8) <>> (1 : Nat8) // => 128
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
Nat8.bittest(5, 2); // => true
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat8, p : Nat) : Nat8
```

Returns the value of setting bit `p mod 8` in `x` to `1`.

Example:
```motoko include=import
Nat8.bitset(5, 1); // => 7
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat8, p : Nat) : Nat8
```

Returns the value of clearing bit `p mod 8` in `x` to `0`.

Example:
```motoko include=import
Nat8.bitclear(5, 2); // => 1
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat8, p : Nat) : Nat8
```

Returns the value of flipping bit `p mod 8` in `x`.

Example:
```motoko include=import
Nat8.bitflip(5, 2); // => 1
```

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Nat8) -> Nat8
```

Returns the count of non-zero bits in `x`.

Example:
```motoko include=import
Nat8.bitcountNonZero(5); // => 2
```

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Nat8) -> Nat8
```

Returns the count of leading zero bits in `x`.

Example:
```motoko include=import
Nat8.bitcountLeadingZero(5); // => 5
```

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Nat8) -> Nat8
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko include=import
Nat8.bitcountTrailingZero(6); // => 1
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat8, y : Nat8) : Nat8
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat8.addWrap(230, 26); // => 0
(230 : Nat8) +% (26 : Nat8) // => 0
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
ignore Nat8.subWrap(0, 1); // => 255
(0 : Nat8) -% (1 : Nat8) // => 255
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
ignore Nat8.mulWrap(230, 26); // => 92
(230 : Nat8) *% (26 : Nat8) // => 92
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
ignore Nat8.powWrap(2, 8); // => 0
(2 : Nat8) **% (8 : Nat8) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `**%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**%`
as a function value at the moment.
