# Nat32
32-bit unsigned integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Nat32`
`type Nat32 = Prim.Types.Nat32`

32-bit natural numbers.

## Value `toNat`
`let toNat : Nat32 -> Nat`

Conversion.

## Value `fromNat`
`let fromNat : Nat -> Nat32`

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
`let fromIntWrap : Int -> Nat32`

Conversion. Wraps on overflow/underflow.

## Function `toText`
`func toText(x : Nat32) : Text`

Returns the Text representation of `x`.

## Function `min`
`func min(x : Nat32, y : Nat32) : Nat32`

Returns the minimum of `x` and `y`.

## Function `max`
`func max(x : Nat32, y : Nat32) : Nat32`

Returns the maximum of `x` and `y`.

## Function `equal`
`func equal(x : Nat32, y : Nat32) : Bool`

Returns `x == y`.

## Function `notEqual`
`func notEqual(x : Nat32, y : Nat32) : Bool`

Returns `x != y`.

## Function `less`
`func less(x : Nat32, y : Nat32) : Bool`

Returns `x < y`.

## Function `lessOrEqual`
`func lessOrEqual(x : Nat32, y : Nat32) : Bool`

Returns `x <= y`.

## Function `greater`
`func greater(x : Nat32, y : Nat32) : Bool`

Returns `x > y`.

## Function `greaterOrEqual`
`func greaterOrEqual(x : Nat32, y : Nat32) : Bool`

Returns `x >= y`.

## Function `compare`
`func compare(x : Nat32, y : Nat32) : {#less; #equal; #greater}`

Returns the order of `x` and `y`.

## Function `add`
`func add(x : Nat32, y : Nat32) : Nat32`

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
`func sub(x : Nat32, y : Nat32) : Nat32`

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
`func mul(x : Nat32, y : Nat32) : Nat32`

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
`func div(x : Nat32, y : Nat32) : Nat32`

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
`func rem(x : Nat32, y : Nat32) : Nat32`

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
`func pow(x : Nat32, y : Nat32) : Nat32`

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
`func bitnot(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
`func bitand(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
`func bitor(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
`func bitxor(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
`func bitshiftLeft(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
`func bitshiftRight(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
`func bitrotLeft(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
`func bitrotRight(x : Nat32, y : Nat32) : Nat32`

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
`func bittest(x : Nat32, p : Nat) : Bool`

Returns the value of bit `p mod 32` in `x`, `(x & 2^(p mod 32)) == 2^(p mod 32)`.

## Function `bitset`
`func bitset(x : Nat32, p : Nat) : Nat32`

Returns the value of setting bit `p mod 32` in `x` to `1`.

## Function `bitclear`
`func bitclear(x : Nat32, p : Nat) : Nat32`

Returns the value of clearing bit `p mod 32` in `x` to `0`.

## Function `bitflip`
`func bitflip(x : Nat32, p : Nat) : Nat32`

Returns the value of flipping bit `p mod 32` in `x`.

## Value `bitcountNonZero`
`let bitcountNonZero : (x : Nat32) -> Nat32`

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
`let bitcountLeadingZero : (x : Nat32) -> Nat32`

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
`let bitcountTrailingZero : (x : Nat32) -> Nat32`

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
`func addWrap(x : Nat32, y : Nat32) : Nat32`

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
`func subWrap(x : Nat32, y : Nat32) : Nat32`

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
`func mulWrap(x : Nat32, y : Nat32) : Nat32`

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
`func powWrap(x : Nat32, y : Nat32) : Nat32`

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
