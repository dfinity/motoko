# Int
Signed integer numbers with infinite precision (also called big integers).

Most operations on integer numbers (e.g. addition) are available as built-in operators (e.g. `-1 + 1`).
This module provides equivalent functions and `Text` conversion.

Import from the base library to use this module.
```motoko name=import
import Int "mo:base/Int";
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
Int.abs(-12) // => 12
```

## Function `toText`
``` motoko no-repl
func toText(x : Int) : Text
```

Converts an integer number to its textual representation. Textual
representation _do not_ contain underscores to represent commas.

Example:
```motoko include=import
Int.toText(-1234) // => "-1234"
```

## Function `min`
``` motoko no-repl
func min(x : Int, y : Int) : Int
```

Returns the minimum of `x` and `y`.

Example:
```motoko include=import
Int.min(2, -3) // => -3
```

## Function `max`
``` motoko no-repl
func max(x : Int, y : Int) : Int
```

Returns the maximum of `x` and `y`.

Example:
```motoko include=import
Int.max(2, -3) // => 2
```

## Function `hash`
``` motoko no-repl
func hash(i : Int) : Hash.Hash
```

Computes a hash from the least significant 32-bits of `i`, ignoring other bits.
@deprecated For large `Int` values consider using a bespoke hash function that considers all of the argument's bits.

## Function `hashAcc`
``` motoko no-repl
func hashAcc(h1 : Hash.Hash, i : Int) : Hash.Hash
```

Computes an accumulated hash from `h1` and the least significant 32-bits of `i`, ignoring other bits in `i`.
@deprecated For large `Int` values consider using a bespoke hash function that considers all of the argument's bits.

## Function `equal`
``` motoko no-repl
func equal(x : Int, y : Int) : Bool
```

Equality function for Int types.
This is equivalent to `x == y`.

Example:
```motoko include=import
Int.equal(-1, -1); // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `==` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `==`
as a function value at the moment.

Example:
```motoko include=import
import Buffer "mo:base/Buffer";

let buffer1 = Buffer.Buffer<Int>(1);
buffer1.add(-3);
let buffer2 = Buffer.Buffer<Int>(1);
buffer2.add(-3);
Buffer.equal(buffer1, buffer2, Int.equal) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int, y : Int) : Bool
```

Inequality function for Int types.
This is equivalent to `x != y`.

Example:
```motoko include=import
Int.notEqual(-1, -2); // => true
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
Int.less(-2, 1); // => true
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
Int.lessOrEqual(-2, 1); // => true
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
Int.greater(1, -2); // => true
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
Int.greaterOrEqual(1, -2); // => true
```

Note: The reason why this function is defined in this library (in addition
to the existing `>=` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `>=`
as a function value at the moment.

## Function `compare`
``` motoko no-repl
func compare(x : Int, y : Int) : {#less; #equal; #greater}
```

General-purpose comparison function for `Int`. Returns the `Order` (
either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.

Example:
```motoko include=import
Int.compare(-3, 2) // => #less
```

This function can be used as value for a high order function, such as a sort function.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.sort([1, -2, -3], Int.compare) // => [-3, -2, 1]
```

## Function `neg`
``` motoko no-repl
func neg(x : Int) : Int
```

Returns the negation of `x`, `-x` .

Example:
```motoko include=import
Int.neg(123) // => -123
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
Int.add(1, -2); // => -1
```

Note: The reason why this function is defined in this library (in addition
to the existing `+` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `+`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([1, -2, -3], 0, Int.add) // => -4
```

## Function `sub`
``` motoko no-repl
func sub(x : Int, y : Int) : Int
```

Returns the difference of `x` and `y`, `x - y`.

No overflow since `Int` has infinite precision.

Example:
```motoko include=import
Int.sub(1, 2); // => -1
```

Note: The reason why this function is defined in this library (in addition
to the existing `-` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `-`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([1, -2, -3], 0, Int.sub) // => 4
```

## Function `mul`
``` motoko no-repl
func mul(x : Int, y : Int) : Int
```

Returns the product of `x` and `y`, `x * y`.

No overflow since `Int` has infinite precision.

Example:
```motoko include=import
Int.mul(-2, 3); // => -6
```

Note: The reason why this function is defined in this library (in addition
to the existing `*` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `*`
as a function value at the moment.

Example:
```motoko include=import
import Array "mo:base/Array";
Array.foldLeft([1, -2, -3], 1, Int.mul) // => 6
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
Int.div(6, -2); // => -3
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
Int.rem(6, -4); // => 2
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
Int.pow(-2, 3); // => -8
```

Note: The reason why this function is defined in this library (in addition
to the existing `**` operator) is so that you can use it as a function
value to pass to a higher order function. It is not possible to use `**`
as a function value at the moment.
