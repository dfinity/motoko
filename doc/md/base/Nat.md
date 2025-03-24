# Nat
Natural numbers with infinite precision.

Most operations on natural numbers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

Import from the base library to use this module.
```motoko name=import
import Nat "mo:base/Nat";
```

## Type `Nat`
``` motoko no-repl
type Nat = Prim.Types.Nat
```

Infinite precision natural numbers.

## Function `toText`
``` motoko no-repl
func toText(n : Nat) : Text
```

Converts a natural number to its textual representation. Textual
representation _do not_ contain underscores to represent commas.

Example:
```motoko include=import
Nat.toText 1234 // => "1234"
```

## Function `fromText`
``` motoko no-repl
func fromText(text : Text) : ?Nat
```

Creates a natural number from its textual representation. Returns `null`
if the input is not a valid natural number.

Note: The textual representation _must not_ contain underscores.

Example:
```motoko include=import
Nat.fromText "1234" // => ?1234
```

## Function `min`
``` motoko no-repl
func min(x : Nat, y : Nat) : Nat
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
Nat.min(1, 2) // => 1
```

## Function `max`
``` motoko no-repl
func max(x : Nat, y : Nat) : Nat
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
Nat.max(1, 2) // => 2
```

## Function `equal`
``` motoko no-repl
func equal(x : Nat, y : Nat) : Bool
```

Equality function for Nat types.
This is equivalent to `x == y`.

Example:
```motoko include=import
ignore Nat.equal(1, 1); // => true
1 == 1 // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
import Buffer "mo:base/Buffer";

let buffer1 = Buffer.Buffer<Nat>(3);
let buffer2 = Buffer.Buffer<Nat>(3);
Buffer.equal(buffer1, buffer2, Nat.equal) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat, y : Nat) : Bool
```

Inequality function for Nat types.
This is equivalent to `x != y`.

Example:
```motoko include=import
ignore Nat.notEqual(1, 2); // => true
1 != 2 // => true
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
ignore Nat.less(1, 2); // => true
1 < 2 // => true
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
ignore Nat.lessOrEqual(1, 2); // => true
1 <= 2 // => true
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
ignore Nat.greater(2, 1); // => true
2 > 1 // => true
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
ignore Nat.greaterOrEqual(2, 1); // => true
2 >= 1 // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Nat, y : Nat) : {#less; #equal; #greater}
```

General purpose comparison function for `Nat`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
Nat.compare(2, 3) // => #less
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.sort([2, 3, 1], Nat.compare) // => [1, 2, 3]
```

## Function `add`
``` motoko no-repl
func add(x : Nat, y : Nat) : Nat
```

Returns the sum of `x` and `y`, `x + y`. This operator will never overflow
because `Nat` is infinite precision.

Example:
```motoko include=import
ignore Nat.add(1, 2); // => 3
1 + 2 // => 3
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([2, 3, 1], 0, Nat.add) // => 6
```

## Function `sub`
``` motoko no-repl
func sub(x : Nat, y : Nat) : Nat
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow below `0`.

Example:
```motoko include=import
ignore Nat.sub(2, 1); // => 1
// Add a type annotation to avoid a warning about the subtraction
2 - 1 : Nat // => 1
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([2, 3, 1], 10, Nat.sub) // => 4
```

## Function `mul`
``` motoko no-repl
func mul(x : Nat, y : Nat) : Nat
```

Returns the product of `x` and `y`, `x * y`. This operator will never
overflow because `Nat` is infinite precision.

Example:
```motoko include=import
ignore Nat.mul(2, 3); // => 6
2 * 3 // => 6
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([2, 3, 1], 1, Nat.mul) // => 6
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
ignore Nat.div(6, 2); // => 3
6 / 2 // => 3
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
ignore Nat.rem(6, 4); // => 2
6 % 4 // => 2
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
ignore Nat.pow(2, 3); // => 8
2 ** 3 // => 8
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
Nat.bitshiftLeft(1, 3); // => 8
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
Nat.bitshiftRight(8, 3); // => 1
```

Note: The reason why this function is defined in this library (in absence
of the `>>` operator) is so that you can use it as a function
value to pass to a higher order function. While `Nat` is not defined in terms
of bit patterns, conceptually it can be regarded as such, and the operation
is provided as a high-performance version of the corresponding arithmetic
rule.
