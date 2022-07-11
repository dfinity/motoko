# Nat
Natural numbers

Most operations on natural numbers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

## Type `Nat`
``` motoko norepl
type Nat = Prim.Types.Nat
```

Infinite precision natural numbers.

## Value `toText`
``` motoko norepl
let toText : Nat -> Text
```

Conversion.

## Function `min`
``` motoko norepl
func min(x : Nat, y : Nat) : Nat
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko norepl
func max(x : Nat, y : Nat) : Nat
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko norepl
func equal(x : Nat, y : Nat) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Nat, y : Nat) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Nat, y : Nat) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Nat, y : Nat) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Nat, y : Nat) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Nat, y : Nat) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Nat, y : Nat) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `add`
``` motoko norepl
func add(x : Nat, y : Nat) : Nat
```

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
``` motoko norepl
func sub(x : Nat, y : Nat) : Nat
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

## Function `mul`
``` motoko norepl
func mul(x : Nat, y : Nat) : Nat
```

Returns the product of `x` and `y`, `x * y`.

## Function `div`
``` motoko norepl
func div(x : Nat, y : Nat) : Nat
```

Returns the division of `x` by `y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko norepl
func rem(x : Nat, y : Nat) : Nat
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko norepl
func pow(x : Nat, y : Nat) : Nat
```

Returns `x` to the power of `y`, `x ** y`.
