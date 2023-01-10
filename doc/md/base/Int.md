# Int
Integer numbers

Most operations on integers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

## Type `Int`
``` motoko no-repl
type Int = Prim.Types.Int
```

Infinite precision signed integers.

## Value `abs`
``` motoko no-repl
let abs : Int -> Nat
```

Returns the absolute value of the number

## Value `toText`
``` motoko no-repl
let toText : Int -> Text
```

Conversion.

## Function `min`
``` motoko no-repl
func min(x : Int, y : Int) : Int
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko no-repl
func max(x : Int, y : Int) : Int
```

Returns the maximum of `x` and `y`.

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

@deprecated This function will be removed in future.

## Function `equal`
``` motoko no-repl
func equal(x : Int, y : Int) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int, y : Int) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko no-repl
func less(x : Int, y : Int) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Int, y : Int) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko no-repl
func greater(x : Int, y : Int) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Int, y : Int) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko no-repl
func compare(x : Int, y : Int) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `neq`
``` motoko no-repl
func neq(x : Int) : Int
```

Returns the negation of `x`, `-x` .

## Function `add`
``` motoko no-repl
func add(x : Int, y : Int) : Int
```

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
``` motoko no-repl
func sub(x : Int, y : Int) : Int
```

Returns the difference of `x` and `y`, `x - y`.

## Function `mul`
``` motoko no-repl
func mul(x : Int, y : Int) : Int
```

Returns the product of `x` and `y`, `x * y`.

## Function `div`
``` motoko no-repl
func div(x : Int, y : Int) : Int
```

Returns the division of `x` by `y`,  `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko no-repl
func rem(x : Int, y : Int) : Int
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko no-repl
func pow(x : Int, y : Int) : Int
```

Returns `x` to the power of `y`, `x ** y`.
