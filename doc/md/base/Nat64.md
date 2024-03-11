# Nat64
Provides utility functions on 64-bit unsigned integers.

Note that most operations are available as built-in operators (e.g. `1 + 1`).

Import from the base library to use this module.
```motoko name=import
import Nat64 "mo:base/Nat64";
```

## Type `Nat64`
``` motoko no-repl
type Nat64 = Prim.Types.Nat64
```

64-bit natural numbers.

## Value `maximumValue`
``` motoko no-repl
let maximumValue : Nat64
```

Maximum 64-bit natural number. `2 ** 64 - 1`.

Example:
```motoko include=import
Nat64.maximumValue; // => 18446744073709551615 : Nat64
```

## Value `toNat`
``` motoko no-repl
let toNat : Nat64 -> Nat
```

Converts a 64-bit unsigned integer to an unsigned integer with infinite precision.

Example:
```motoko include=import
Nat64.toNat(123); // => 123 : Nat
```

## Value `fromNat`
``` motoko no-repl
let fromNat : Nat -> Nat64
```

Converts an unsigned integer with infinite precision to a 64-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat64.fromNat(123); // => 123 : Nat64
```

## Function `fromNat32`
``` motoko no-repl
func fromNat32(x : Nat32) : Nat64
```

Converts a 32-bit unsigned integer to a 64-bit unsigned integer.

Example:
```motoko include=import
Nat64.fromNat32(123); // => 123 : Nat64
```

## Function `toNat32`
``` motoko no-repl
func toNat32(x : Nat64) : Nat32
```

Converts a 64-bit unsigned integer to a 32-bit unsigned integer.

Traps on overflow.

Example:
```motoko include=import
Nat64.toNat32(123); // => 123 : Nat32
```

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Nat64
```

Converts a signed integer with infinite precision to a 64-bit unsigned integer.

Traps on overflow/underflow.

Example:
```motoko include=import
Nat64.fromIntWrap(123); // => 123 : Nat64
```

## Function `toText`
``` motoko no-repl
func toText(x : Nat64) : Text
```

Converts `x` to its textual representation. Textual representation _do not_
contain underscores to represent commas.

Example:
```motoko include=import
Nat64.toText(1234); // => "1234" : Text
```

## Function `min`
``` motoko no-repl
func min(x : Nat64, y : Nat64) : Nat64
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
Nat64.min(123, 456); // => 123 : Nat64
```

## Function `max`
``` motoko no-repl
func max(x : Nat64, y : Nat64) : Nat64
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
Nat64.max(123, 456); // => 456 : Nat64
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat64, y : Nat64) : Bool
```

Equality function for Nat64 types.
This is equivalent to `x == y`.

Example:
```motoko include=import
ignore Nat64.equal(1, 1); // => true
(1 : Nat64) == (1 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
import Buffer "mo:base/Buffer";

let buffer1 = Buffer.Buffer<Nat64>(3);
let buffer2 = Buffer.Buffer<Nat64>(3);
Buffer.equal(buffer1, buffer2, Nat64.equal) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat64, y : Nat64) : Bool
```

Inequality function for Nat64 types.
This is equivalent to `x != y`.

Example:
```motoko include=import
ignore Nat64.notEqual(1, 2); // => true
(1 : Nat64) != (2 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `!=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `!=`
as a function value at the moment.

## Function `less`
``` motoko no-repl
func less(x : Nat64, y : Nat64) : Bool
```

"Less than" function for Nat64 types.
This is equivalent to `x < y`.

Example:
```motoko include=import
ignore Nat64.less(1, 2); // => true
(1 : Nat64) < (2 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<`
as a function value at the moment.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat64, y : Nat64) : Bool
```

"Less than or equal" function for Nat64 types.
This is equivalent to `x <= y`.

Example:
```motoko include=import
ignore Nat64.lessOrEqual(1, 2); // => true
(1 : Nat64) <= (2 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `<=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<=`
as a function value at the moment.

## Function `greater`
``` motoko no-repl
func greater(x : Nat64, y : Nat64) : Bool
```

"Greater than" function for Nat64 types.
This is equivalent to `x > y`.

Example:
```motoko include=import
ignore Nat64.greater(2, 1); // => true
(2 : Nat64) > (1 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>`
as a function value at the moment.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat64, y : Nat64) : Bool
```

"Greater than or equal" function for Nat64 types.
This is equivalent to `x >= y`.

Example:
```motoko include=import
ignore Nat64.greaterOrEqual(2, 1); // => true
(2 : Nat64) >= (1 : Nat64) // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat64, y : Nat64) : {#less; #equal; #greater}
```

General purpose comparison function for `Nat64`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
Nat64.compare(2, 3) // => #less
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.sort([2, 3, 1] : [Nat64], Nat64.compare) // => [1, 2, 3]
```

## Function `add`
``` motoko no-repl
func add(x : Nat64, y : Nat64) : Nat64
```

Returns the sum of `x` and `y`, `x + y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat64.add(1, 2); // => 3
(1 : Nat64) + (2 : Nat64) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat64, Nat64>([2, 3, 1], 0, Nat64.add) // => 6
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat64, y : Nat64) : Nat64
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

Example:
```motoko include=import
ignore Nat64.sub(3, 1); // => 2
(3 : Nat64) - (1 : Nat64) // => 2
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat64, Nat64>([2, 3, 1], 10, Nat64.sub) // => 4
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat64, y : Nat64) : Nat64
```

Returns the product of `x` and `y`, `x * y`.
Traps on overflow.

Example:
```motoko include=import
ignore Nat64.mul(2, 3); // => 6
(2 : Nat64) * (3 : Nat64) // => 6
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft<Nat64, Nat64>([2, 3, 1], 1, Nat64.mul) // => 6
```

## Function `div`
``` motoko no-repl
func div(x : Nat64, y : Nat64) : Nat64
```

Returns the quotient of `x` divided by `y`, `x / y`.
Traps when `y` is zero.

Example:
```motoko include=import
ignore Nat64.div(6, 2); // => 3
(6 : Nat64) / (2 : Nat64) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `/` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `/`
as a function value at the moment.

## Function `rem`
``` motoko no-repl
func rem(x : Nat64, y : Nat64) : Nat64
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

Example:
```motoko include=import
ignore Nat64.rem(6, 4); // => 2
(6 : Nat64) % (4 : Nat64) // => 2
```

Note: The reason why this function is defined in this library (in addition
to the existing `%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `%`
as a function value at the moment.

## Function `pow`
``` motoko no-repl
func pow(x : Nat64, y : Nat64) : Nat64
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

Example:
```motoko include=import
ignore Nat64.pow(2, 3); // => 8
(2 : Nat64) ** (3 : Nat64) // => 8
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Nat64) : Nat64
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko include=import
ignore Nat64.bitnot(0); // => 18446744073709551615
^(0 : Nat64) // => 18446744073709551615
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitand`
``` motoko no-repl
func bitand(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise and of `x` and `y`, `x & y`.

Example:
```motoko include=import
ignore Nat64.bitand(1, 3); // => 1
(1 : Nat64) & (3 : Nat64) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `&` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `&`
as a function value at the moment.

## Function `bitor`
``` motoko no-repl
func bitor(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise or of `x` and `y`, `x | y`.

Example:
```motoko include=import
ignore Nat64.bitor(1, 3); // => 3
(1 : Nat64) | (3 : Nat64) // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `|` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `|`
as a function value at the moment.

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

Example:
```motoko include=import
ignore Nat64.bitxor(1, 3); // => 2
(1 : Nat64) ^ (3 : Nat64) // => 2
```

Note: The reason why this function is defined in this library (in addition
to the existing `^` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `^`
as a function value at the moment.

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

Example:
```motoko include=import
ignore Nat64.bitshiftLeft(1, 3); // => 8
(1 : Nat64) << (3 : Nat64) // => 8
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<`
as a function value at the moment.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

Example:
```motoko include=import
ignore Nat64.bitshiftRight(8, 3); // => 1
(8 : Nat64) >> (3 : Nat64) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>>`
as a function value at the moment.

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

Example:
```motoko include=import
ignore Nat64.bitrotLeft(1, 3); // => 8
(1 : Nat64) <<> (3 : Nat64) // => 8
```

Note: The reason why this function is defined in this library (in addition
to the existing `<<>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<<>`
as a function value at the moment.

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

Example:
```motoko include=import
ignore Nat64.bitrotRight(8, 3); // => 1
(8 : Nat64) <>> (3 : Nat64) // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `<>>` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `<>>`
as a function value at the moment.

## Function `bittest`
``` motoko no-repl
func bittest(x : Nat64, p : Nat) : Bool
```

Returns the value of bit `p mod 64` in `x`, `(x & 2^(p mod 64)) == 2^(p mod 64)`.
This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.

Example:
```motoko include=import
Nat64.bittest(5, 2); // => true
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat64, p : Nat) : Nat64
```

Returns the value of setting bit `p mod 64` in `x` to `1`.

Example:
```motoko include=import
Nat64.bitset(5, 1); // => 7
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat64, p : Nat) : Nat64
```

Returns the value of clearing bit `p mod 64` in `x` to `0`.

Example:
```motoko include=import
Nat64.bitclear(5, 2); // => 1
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat64, p : Nat) : Nat64
```

Returns the value of flipping bit `p mod 64` in `x`.

Example:
```motoko include=import
Nat64.bitflip(5, 2); // => 1
```

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Nat64) -> Nat64
```

Returns the count of non-zero bits in `x`.

Example:
```motoko include=import
Nat64.bitcountNonZero(5); // => 2
```

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Nat64) -> Nat64
```

Returns the count of leading zero bits in `x`.

Example:
```motoko include=import
Nat64.bitcountLeadingZero(5); // => 61
```

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Nat64) -> Nat64
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko include=import
Nat64.bitcountTrailingZero(16); // => 4
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat64.addWrap(Nat64.maximumValue, 1); // => 0
Nat64.maximumValue +% (1 : Nat64) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `+%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+%`
as a function value at the moment.

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

Example:
```motoko include=import
ignore Nat64.subWrap(0, 1); // => 18446744073709551615
(0 : Nat64) -% (1 : Nat64) // => 18446744073709551615
```

Note: The reason why this function is defined in this library (in addition
to the existing `-%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-%`
as a function value at the moment.

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat64.mulWrap(4294967296, 4294967296); // => 0
(4294967296 : Nat64) *% (4294967296 : Nat64) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `*%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*%`
as a function value at the moment.

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Nat64, y : Nat64) : Nat64
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.

Example:
```motoko include=import
ignore Nat64.powWrap(2, 64); // => 0
(2 : Nat64) **% (64 : Nat64) // => 0
```

Note: The reason why this function is defined in this library (in addition
to the existing `**%` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**%`
as a function value at the moment.
