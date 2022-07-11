# Int
Integer numbers

Most operations on integers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

## Type `Int`
``` motoko norepl
type Int = Prim.Types.Int
```

Infinite precision signed integers.

## Value `abs`
``` motoko norepl
let abs : Int -> Nat
```

Returns the absolute value of the number

## Value `toText`
``` motoko norepl
let toText : Int -> Text
```

Conversion.

## Function `min`
``` motoko norepl
func min(x : Int, y : Int) : Int
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko norepl
func max(x : Int, y : Int) : Int
```

Returns the maximum of `x` and `y`.

## Function `hash`
``` motoko norepl
func hash(i : Int) : Hash.Hash
```

Computes a hash from the least significant 32-bits of `i`, ignoring other bits.
@deprecated For large `Int` values consider using a bespoke hash function that considers all of the argument's bits.

## Function `hashAcc`
``` motoko norepl
func hashAcc(h1 : Hash.Hash, i : Int) : Hash.Hash
```

@deprecated This function will be removed in future.

## Function `equal`
``` motoko norepl
func equal(x : Int, y : Int) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Int, y : Int) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Int, y : Int) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Int, y : Int) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Int, y : Int) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Int, y : Int) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Int, y : Int) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `neq`
``` motoko norepl
func neq(x : Int) : Int
```

Returns the negation of `x`, `-x` .

## Function `add`
``` motoko norepl
func add(x : Int, y : Int) : Int
```

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
``` motoko norepl
func sub(x : Int, y : Int) : Int
```

Returns the difference of `x` and `y`, `x - y`.

## Function `mul`
``` motoko norepl
func mul(x : Int, y : Int) : Int
```

Returns the product of `x` and `y`, `x * y`.

## Function `div`
``` motoko norepl
func div(x : Int, y : Int) : Int
```

Returns the division of `x` by `y`,  `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko norepl
func rem(x : Int, y : Int) : Int
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko norepl
func pow(x : Int, y : Int) : Int
```

Returns `x` to the power of `y`, `x ** y`.
