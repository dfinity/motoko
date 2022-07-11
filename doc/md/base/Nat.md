# Nat
Natural numbers

Most operations on natural numbers (e.g. addition) are available as built-in operators (e.g. `1 + 1`).
This module provides equivalent functions and `Text` conversion.

## Type `Nat`
`type Nat = Prim.Types.Nat`

Infinite precision natural numbers.

## Value `toText`
`let toText : Nat -> Text`

Conversion.

## Function `min`
`func min(x : Nat, y : Nat) : Nat`

Returns the minimum of `x` and `y`.

## Function `max`
`func max(x : Nat, y : Nat) : Nat`

Returns the maximum of `x` and `y`.

## Function `equal`
`func equal(x : Nat, y : Nat) : Bool`

Returns `x == y`.

## Function `notEqual`
`func notEqual(x : Nat, y : Nat) : Bool`

Returns `x != y`.

## Function `less`
`func less(x : Nat, y : Nat) : Bool`

Returns `x < y`.

## Function `lessOrEqual`
`func lessOrEqual(x : Nat, y : Nat) : Bool`

Returns `x <= y`.

## Function `greater`
`func greater(x : Nat, y : Nat) : Bool`

Returns `x > y`.

## Function `greaterOrEqual`
`func greaterOrEqual(x : Nat, y : Nat) : Bool`

Returns `x >= y`.

## Function `compare`
`func compare(x : Nat, y : Nat) : {#less; #equal; #greater}`

Returns the order of `x` and `y`.

## Function `add`
`func add(x : Nat, y : Nat) : Nat`

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
`func sub(x : Nat, y : Nat) : Nat`

Returns the difference of `x` and `y`, `x - y`.
Traps on underflow.

## Function `mul`
`func mul(x : Nat, y : Nat) : Nat`

Returns the product of `x` and `y`, `x * y`.

## Function `div`
`func div(x : Nat, y : Nat) : Nat`

Returns the division of `x` by `y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
`func rem(x : Nat, y : Nat) : Nat`

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
`func pow(x : Nat, y : Nat) : Nat`

Returns `x` to the power of `y`, `x ** y`.
