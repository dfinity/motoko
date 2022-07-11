# Int8
8-bit signed integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int8`
`type Int8 = Prim.Types.Int8`

8-bit signed integers.

## Value `toInt`
`let toInt : Int8 -> Int`

Conversion.

## Value `fromInt`
`let fromInt : Int -> Int8`

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
`let fromIntWrap : Int -> Int8`

Conversion. Wraps on overflow/underflow.

## Value `fromNat8`
`let fromNat8 : Nat8 -> Int8`

Conversion. Wraps on overflow/underflow.

## Value `toNat8`
`let toNat8 : Int8 -> Nat8`

Conversion. Wraps on overflow/underflow.

## Function `toText`
`func toText(x : Int8) : Text`

Returns the Text representation of `x`.

## Function `abs`
`func abs(x : Int8) : Int8`

Returns the absolute value of `x`. Traps when `x = -2^7`.

## Function `min`
`func min(x : Int8, y : Int8) : Int8`

Returns the minimum of `x` and `y`.

## Function `max`
`func max(x : Int8, y : Int8) : Int8`

Returns the maximum of `x` and `y`.

## Function `equal`
`func equal(x : Int8, y : Int8) : Bool`

Returns `x == y`.

## Function `notEqual`
`func notEqual(x : Int8, y : Int8) : Bool`

Returns `x != y`.

## Function `less`
`func less(x : Int8, y : Int8) : Bool`

Returns `x < y`.

## Function `lessOrEqual`
`func lessOrEqual(x : Int8, y : Int8) : Bool`

Returns `x <= y`.

## Function `greater`
`func greater(x : Int8, y : Int8) : Bool`

Returns `x > y`.

## Function `greaterOrEqual`
`func greaterOrEqual(x : Int8, y : Int8) : Bool`

Returns `x >= y`.

## Function `compare`
`func compare(x : Int8, y : Int8) : {#less; #equal; #greater}`

Returns the order of `x` and `y`.

## Function `neg`
`func neg(x : Int8) : Int8`

Returns the negation of `x`, `-x`. Traps on overflow.

## Function `add`
`func add(x : Int8, y : Int8) : Int8`

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
`func sub(x : Int8, y : Int8) : Int8`

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
`func mul(x : Int8, y : Int8) : Int8`

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
`func div(x : Int8, y : Int8) : Int8`

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
`func rem(x : Int8, y : Int8) : Int8`

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
`func pow(x : Int8, y : Int8) : Int8`

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
`func bitnot(x : Int8, y : Int8) : Int8`

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
`func bitand(x : Int8, y : Int8) : Int8`

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
`func bitor(x : Int8, y : Int8) : Int8`

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
`func bitxor(x : Int8, y : Int8) : Int8`

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
`func bitshiftLeft(x : Int8, y : Int8) : Int8`

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
`func bitshiftRight(x : Int8, y : Int8) : Int8`

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
`func bitrotLeft(x : Int8, y : Int8) : Int8`

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
`func bitrotRight(x : Int8, y : Int8) : Int8`

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
`func bittest(x : Int8, p : Nat) : Bool`

Returns the value of bit `p mod 8` in `x`, `(x & 2^(p mod 8)) == 2^(p mod 8)`.

## Function `bitset`
`func bitset(x : Int8, p : Nat) : Int8`

Returns the value of setting bit `p mod 8` in `x` to `1`.

## Function `bitclear`
`func bitclear(x : Int8, p : Nat) : Int8`

Returns the value of clearing bit `p mod 8` in `x` to `0`.

## Function `bitflip`
`func bitflip(x : Int8, p : Nat) : Int8`

Returns the value of flipping bit `p mod 8` in `x`.

## Value `bitcountNonZero`
`let bitcountNonZero : (x : Int8) -> Int8`

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
`let bitcountLeadingZero : (x : Int8) -> Int8`

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
`let bitcountTrailingZero : (x : Int8) -> Int8`

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
`func addWrap(x : Int8, y : Int8) : Int8`

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
`func subWrap(x : Int8, y : Int8) : Int8`

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
`func mulWrap(x : Int8, y : Int8) : Int8`

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
`func powWrap(x : Int8, y : Int8) : Int8`

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
