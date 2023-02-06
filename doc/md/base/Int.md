# Int
Signed integer numbers with infinite precision (also called big integers).

Common integer functions.
Most operations on integers (e.g. addition) are also available as built-in operators (e.g. `1 + 1`).

## Type `Int`
``` motoko no-repl
type Int = Prim.Types.Int
```

Infinite precision signed integers.

## Value `abs`
``` motoko no-repl
let abs : (x : Int) -> Nat
```

Returns the absolute value of `x`.

Example:
```motoko
import Int "mo:base/Int";

Int.abs(-12) // => 12
```

## Value `toText`
``` motoko no-repl
let toText : Int -> Text
```

Conversion to Text.
Formats the integer in decimal representation without underscore separators for blocks of thousands.

Example:
```motoko
import Int "mo:base/Int";

Int.toText(-1234) // => "-1234"
```

## Function `min`
``` motoko no-repl
func min(x : Int, y : Int) : Int
```

Returns the minimum of `x` and `y`.

Example:
```motoko
import Int "mo:base/Int";

Int.min(+2, -3) // => -3
```

## Function `max`
``` motoko no-repl
func max(x : Int, y : Int) : Int
```

Returns the maximum of `x` and `y`.

Example:
```motoko
import Int "mo:base/Int";

Int.max(+2, -3) // => 2
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

Returns `x == y`.

Example:
```motoko
import Int "mo:base/Int";

Int.equal(123, 123) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int, y : Int) : Bool
```

Returns `x != y`.

Example:
```motoko
import Int "mo:base/Int";

Int.notEqual(123, 123) // => false
```

## Function `less`
``` motoko no-repl
func less(x : Int, y : Int) : Bool
```

Returns `x < y`.

Example:
```motoko
import Int "mo:base/Int";

Int.less(123, 1234) // => true
```

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Int, y : Int) : Bool
```

Returns `x <= y`.

Example:
```motoko
import Int "mo:base/Int";

Int.lessOrEqual(123, 1234) // => true
```

## Function `greater`
``` motoko no-repl
func greater(x : Int, y : Int) : Bool
```

Returns `x > y`.

Example:
```motoko
import Int "mo:base/Int";

Int.greater(1234, 123) // => true
```

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Int, y : Int) : Bool
```

Returns `x >= y`.

Example:
```motoko
import Int "mo:base/Int";

Int.greaterOrEqual(1234, 123) // => true
```

## Function `compare`
``` motoko no-repl
func compare(x : Int, y : Int) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

Example:
```motoko
import Int "mo:base/Int";

Int.compare(123, 1234) // => #less
```

## Function `neg`
``` motoko no-repl
func neg(x : Int) : Int
```

Returns the negation of `x`, `-x` .

Example:
```motoko
import Int "mo:base/Int";

Int.neg(123) // => -123
```

## Function `add`
``` motoko no-repl
func add(x : Int, y : Int) : Int
```

Returns the sum of `x` and `y`, `x + y`.

No overflow since `Int` has infinite precision.

Example:
```motoko
import Int "mo:base/Int";

Int.add(1234, 123) // => 1_357
```

## Function `sub`
``` motoko no-repl
func sub(x : Int, y : Int) : Int
```

Returns the difference of `x` and `y`, `x - y`.

No overflow since `Int` has infinite precision.

Example:
```motoko
import Int "mo:base/Int";

Int.sub(1234, 123) // => 1_111
```

## Function `mul`
``` motoko no-repl
func mul(x : Int, y : Int) : Int
```

Returns the product of `x` and `y`, `x * y`.

No overflow since `Int` has infinite precision.

Example:
```motoko
import Int "mo:base/Int";

Int.mul(123, 100) // => 12_300
```

## Function `div`
``` motoko no-repl
func div(x : Int, y : Int) : Int
```

Returns the signed integer division of `x` by `y`,  `x / y`.
Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.

Traps when `y` is zero.

Example:
```motoko
import Int "mo:base/Int";

Int.div(123, 10) // => 12
```

## Function `rem`
``` motoko no-repl
func rem(x : Int, y : Int) : Int
```

Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
which is defined as `x - x / y * y`.

Traps when `y` is zero.

Example:
```motoko
import Int "mo:base/Int";

Int.rem(123, 10) // => 3
```

## Function `pow`
``` motoko no-repl
func pow(x : Int, y : Int) : Int
```

Returns `x` to the power of `y`, `x ** y`.

Traps when `y` is negative or `y > 2 ** 32 - 1`.
No overflow since `Int` has infinite precision.

Example:
```motoko
import Int "mo:base/Int";

Int.pow(2, 10) // => 1_024
```
