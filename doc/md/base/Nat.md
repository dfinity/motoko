# Nat
Natural numbers

Most operations on natural numbers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

## Type `Nat`
``` motoko no-repl
type Nat = Prim.Types.Nat
```

Infinite precision natural numbers.

## Value `toText`
``` motoko no-repl
let toText : Nat -> Text
```

Conversion.

## Function `min`
``` motoko no-repl
func min(x : Nat, y : Nat) : Nat
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko no-repl
func max(x : Nat, y : Nat) : Nat
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko no-repl
func equal(x : Nat, y : Nat) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat, y : Nat) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko no-repl
func less(x : Nat, y : Nat) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat, y : Nat) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko no-repl
func greater(x : Nat, y : Nat) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat, y : Nat) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko no-repl
func compare(x : Nat, y : Nat) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `add`
``` motoko no-repl
func add(x : Nat, y : Nat) : Nat
```

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
``` motoko no-repl
func sub(x : Nat, y : Nat) : Nat
```

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

## Function `mul`
``` motoko no-repl
func mul(x : Nat, y : Nat) : Nat
```

Returns the product of `x` and `y`, `x * y`.

## Function `div`
``` motoko no-repl
func div(x : Nat, y : Nat) : Nat
```

Returns the division of `x` by `y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko no-repl
func rem(x : Nat, y : Nat) : Nat
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko no-repl
func pow(x : Nat, y : Nat) : Nat
```

Returns `x` to the power of `y`, `x ** y`.
