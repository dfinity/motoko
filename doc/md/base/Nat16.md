# Nat16
Provides utility functions on 16-bit unsigned integers.

Note that most operations are available as built-in operators (e.g. `1 + 1`).

Import from the base library to use this module.
```motoko name=import
import Nat16 "mo:base/Nat16";
```

## Type `Nat16`
``` motoko no-repl
type Nat16 = Prim.Types.Nat16
```

16-bit natural numbers.

## Value `maximumValue`
``` motoko no-repl
let maximumValue : Nat16
```

Maximum 16-bit natural number. `2 ** 16 - 1`.

Example:
```motoko include=import
Nat16.maximumValue; // => 65536 : Nat16
```

## Value `toNat`
``` motoko no-repl
let toNat : Nat16 -> Nat
```

Converts a 16-bit unsigned integer to an unsigned integer with infinite precision.

Example:
```motoko include=import
Nat16.toNat(123); // => 123 : Nat
```

## Value `fromNat`
``` motoko no-repl
let fromNat : Nat -> Nat16
```

Converts an unsigned integer with infinite precision to a 16-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat16.fromNat(123); // => 123 : Nat16
```

## Function `fromNat8`
``` motoko no-repl
func fromNat8(x : Nat8) : Nat16
```

Converts an 8-bit unsigned integer to a 16-bit unsigned integer.

Example:
```motoko include=import
Nat16.fromNat8(123); // => 123 : Nat16
```

## Function `toNat8`
``` motoko no-repl
func toNat8(x : Nat16) : Nat8
```

Converts a 16-bit unsigned integer to an 8-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat16.toNat8(123); // => 123 : Nat8
```

## Function `fromNat32`
``` motoko no-repl
func fromNat32(x : Nat32) : Nat16
```

Converts a 32-bit unsigned integer to a 16-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat16.fromNat32(123); // => 123 : Nat16
```

## Function `toNat32`
``` motoko no-repl
func toNat32(x : Nat16) : Nat32
```

Converts a 16-bit unsigned integer to a 32-bit unsigned integer.

Example:
```motoko include=import
Nat16.toNat32(123); // => 123 : Nat32
```

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Nat16
```

Converts a signed integer with infinite precision to a 16-bit unsigned integer.

Wraps on overflow/underflow.

Example:
```motoko include=import
Nat16.fromIntWrap(123 : Int); // => 123 : Nat16
```

## Function `toText`
``` motoko no-repl
func toText(x : Nat16) : Text
```

Converts `x` to its textual representation. Textual representation _do not_
contain underscores to represent commas.

Example:
```motoko include=import
Nat16.toText(1234); // => "1234" : Text
```

## Function `min`
``` motoko no-repl
func min(x : Nat16, y : Nat16) : Nat16
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
Nat16.min(123, 200); // => 123 : Nat16
```

## Function `max`
``` motoko no-repl
func max(x : Nat16, y : Nat16) : Nat16
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
Nat16.max(123, 200); // => 200 : Nat16
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat16, y : Nat16) : Bool
```

Equality function for Nat16 types.
This is equivalent to `x == y`.

Example:
```motoko include=import
ignore Nat16.equal(1, 1); // => true
(1 : Nat16) == (1 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
import Buffer "mo:base/Buffer";

let buffer1 = Buffer.Buffer<Nat16>(3);
let buffer2 = Buffer.Buffer<Nat16>(3);
Buffer.equal(buffer1, buffer2, Nat16.equal) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat16, y : Nat16) : Bool
```

Inequality function for Nat16 types.
This is equivalent to `x != y`.

Example:
```motoko include=import
ignore Nat16.notEqual(1, 2); // => true
(1 : Nat16) != (2 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Nat16, y : Nat16) : Bool
```

"Less than" function for Nat16 types.
This is equivalent to `x < y`.

Example:
```motoko include=import
ignore Nat16.less(1, 2); // => true
(1 : Nat16) < (2 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat16, y : Nat16) : Bool
```

"Less than or equal" function for Nat16 types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
ignore Nat16.lessOrEqual(1, 2); // => true
(1 : Nat16) <= (2 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Nat16, y : Nat16) : Bool
```

"Greater than" function for Nat16 types.
This is equivalent to `x > y`.

Example:
```motoko include=import
ignore Nat16.greater(2, 1); // => true
(2 : Nat16) > (1 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat16, y : Nat16) : Bool
```

"Greater than or equal" function for Nat16 types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
ignore Nat16.greaterOrEqual(2, 1); // => true
(2 : Nat16) >= (1 : Nat16) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat16, y : Nat16) : {#less; #equal; #greater}
```

General purpose comparison function for `Nat16`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
Nat16.compare(2, 3) // => #less
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.sort([2, 3, 1] : [Nat16], Nat16.compare) // => [1, 2, 3]
```

## Function `add`
``` motoko no-repl
func add(x : Nat16, y : Nat16) : Nat16
```

Returns the sum of `x` and `y`, `x + y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat16.add(1, 2); // => 3
(1 : Nat16) + (2 : Nat16) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat16, Nat16>([2, 3, 1], 0, Nat16.add) // => 6
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat16, y : Nat16) : Nat16
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

Example:
```motoko include=import
ignore Nat16.sub(2, 1); // => 1
(2 : Nat16) - (1 : Nat16) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat16, Nat16>([2, 3, 1], 20, Nat16.sub) // => 14
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat16, y : Nat16) : Nat16
```

Returns the product of `x` and `y`, `x * y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat16.mul(2, 3); // => 6
(2 : Nat16) * (3 : Nat16) // => 6
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat16, Nat16>([2, 3, 1], 1, Nat16.mul) // => 6
```

## Function `div`
``` motoko no-repl
func div(x : Nat16, y : Nat16) : Nat16
```

Returns the quotient of `x` divided by `y`, `x / y`.
Traps when `y` is zero.

Example:
```motoko include=import
ignore Nat16.div(6, 2); // => 3
(6 : Nat16) / (2 : Nat16) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Nat16, y : Nat16) : Nat16
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

Example:
```motoko include=import
ignore Nat16.rem(6, 4); // => 2
(6 : Nat16) % (4 : Nat16) // => 2
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Nat16, y : Nat16) : Nat16
```

Returns the power of `x` to `y`, `x ** y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat16.pow(2, 3); // => 8
(2 : Nat16) ** (3 : Nat16) // => 8
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Nat16) : Nat16
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko include=import
ignore Nat16.bitnot(0); // => 65535
^(0 : Nat16) // => 65535
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitand`
``` motoko no-repl
func bitand(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise and of `x` and `y`, `x & y`.

Example:
```motoko include=import
ignore Nat16.bitand(0, 1); // => 0
(0 : Nat16) & (1 : Nat16) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `&` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `&`
as a function value at the moment.

## Function `bitor`
``` motoko no-repl
func bitor(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise or of `x` and `y`, `x | y`.

Example:
```motoko include=import
ignore Nat16.bitor(0, 1); // => 1
(0 : Nat16) | (1 : Nat16) // => 1
```

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

Example:
```motoko include=import
ignore Nat16.bitxor(0, 1); // => 1
(0 : Nat16) ^ (1 : Nat16) // => 1
```

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

Example:
```motoko include=import
ignore Nat16.bitshiftLeft(1, 3); // => 8
(1 : Nat16) << (3 : Nat16) // => 8
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<`
as a function value at the moment.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

Example:
```motoko include=import
ignore Nat16.bitshiftRight(8, 3); // => 1
(8 : Nat16) >> (3 : Nat16) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>>`
as a function value at the moment.

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

Example:
```motoko include=import
ignore Nat16.bitrotLeft(2, 1); // => 4
(2 : Nat16) <<> (1 : Nat16) // => 4
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<>`
as a function value at the moment.

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

Example:
```motoko include=import
ignore Nat16.bitrotRight(1, 1); // => 32768
(1 : Nat16) <>> (1 : Nat16) // => 32768
```

Note: The reason why this function is defined in this library (in addition
to the existing `<>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<>>`
as a function value at the moment.

## Function `bittest`
``` motoko no-repl
func bittest(x : Nat16, p : Nat) : Bool
```

Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.
This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.

Example:
```motoko include=import
Nat16.bittest(5, 2); // => true
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat16, p : Nat) : Nat16
```

Returns the value of setting bit `p mod 16` in `x` to `1`.

Example:
```motoko include=import
Nat16.bitset(0, 2); // => 4
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat16, p : Nat) : Nat16
```

Returns the value of clearing bit `p mod 16` in `x` to `0`.

Example:
```motoko include=import
Nat16.bitclear(5, 2); // => 1
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat16, p : Nat) : Nat16
```

Returns the value of flipping bit `p mod 16` in `x`.

Example:
```motoko include=import
Nat16.bitflip(5, 2); // => 1
```

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Nat16) -> Nat16
```

Returns the count of non-zero bits in `x`.

Example:
```motoko include=import
Nat16.bitcountNonZero(5); // => 2
```

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Nat16) -> Nat16
```

Returns the count of leading zero bits in `x`.

Example:
```motoko include=import
Nat16.bitcountLeadingZero(5); // => 13
```

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Nat16) -> Nat16
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko include=import
Nat16.bitcountTrailingZero(5); // => 0
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat16.addWrap(65532, 5); // => 1
(65532 : Nat16) +% (5 : Nat16) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `+%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+%`
as a function value at the moment.

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

Example:
```motoko include=import
ignore Nat16.subWrap(1, 2); // => 65535
(1 : Nat16) -% (2 : Nat16) // => 65535
```

Note: The reason why this function is defined in this library (in addition
to the existing `-%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-%`
as a function value at the moment.

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat16.mulWrap(655, 101); // => 619
(655 : Nat16) *% (101 : Nat16) // => 619
```

Note: The reason why this function is defined in this library (in addition
to the existing `*%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*%`
as a function value at the moment.

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Nat16, y : Nat16) : Nat16
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat16.powWrap(2, 16); // => 0
(2 : Nat16) **% (16 : Nat16) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `**%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**%`
as a function value at the moment.
