# Int16
16-bit signed integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int16`
`type Int16 = Prim.Types.Int16`

16-bit signed integers

## Value `toInt`
`let toInt : Int16 -> Int`

Conversion.

## Value `fromInt`
`let fromInt : Int -> Int16`

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
`let fromIntWrap : Int -> Int16`

Conversion. Wraps on overflow/underflow.

## Value `fromNat16`
`let fromNat16 : Nat16 -> Int16`

Conversion. Wraps on overflow/underflow.

## Value `toNat16`
`let toNat16 : Int16 -> Nat16`

Conversion. Wraps on overflow/underflow.

## Function `toText`
`func toText(x : Int16) : Text`

Returns the Text representation of `x`.

## Function `abs`
`func abs(x : Int16) : Int16`

Returns the absolute value of `x`. Traps when `x = -2^15`.

## Function `min`
`func min(x : Int16, y : Int16) : Int16`

Returns the minimum of `x` and `y`.

## Function `max`
`func max(x : Int16, y : Int16) : Int16`

Returns the maximum of `x` and `y`.

## Function `equal`
`func equal(x : Int16, y : Int16) : Bool`

Returns `x == y`.

## Function `notEqual`
`func notEqual(x : Int16, y : Int16) : Bool`

Returns `x != y`.

## Function `less`
`func less(x : Int16, y : Int16) : Bool`

Returns `x < y`.

## Function `lessOrEqual`
`func lessOrEqual(x : Int16, y : Int16) : Bool`

Returns `x <= y`.

## Function `greater`
`func greater(x : Int16, y : Int16) : Bool`

Returns `x > y`.

## Function `greaterOrEqual`
`func greaterOrEqual(x : Int16, y : Int16) : Bool`

Returns `x >= y`.

## Function `compare`
`func compare(x : Int16, y : Int16) : {#less; #equal; #greater}`

Returns the order of `x` and `y`.

## Function `neg`
`func neg(x : Int16) : Int16`

Returns the negation of `x`, `-x`. Traps on overflow.

## Function `add`
`func add(x : Int16, y : Int16) : Int16`

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
`func sub(x : Int16, y : Int16) : Int16`

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
`func mul(x : Int16, y : Int16) : Int16`

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
`func div(x : Int16, y : Int16) : Int16`

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
`func rem(x : Int16, y : Int16) : Int16`

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
`func pow(x : Int16, y : Int16) : Int16`

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
`func bitnot(x : Int16, y : Int16) : Int16`

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
`func bitand(x : Int16, y : Int16) : Int16`

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
`func bitor(x : Int16, y : Int16) : Int16`

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
`func bitxor(x : Int16, y : Int16) : Int16`

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
`func bitshiftLeft(x : Int16, y : Int16) : Int16`

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
`func bitshiftRight(x : Int16, y : Int16) : Int16`

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
`func bitrotLeft(x : Int16, y : Int16) : Int16`

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
`func bitrotRight(x : Int16, y : Int16) : Int16`

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
`func bittest(x : Int16, p : Nat) : Bool`

Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.

## Function `bitset`
`func bitset(x : Int16, p : Nat) : Int16`

Returns the value of setting bit `p mod 16` in `x` to `1`.

## Function `bitclear`
`func bitclear(x : Int16, p : Nat) : Int16`

Returns the value of clearing bit `p mod 16` in `x` to `0`.

## Function `bitflip`
`func bitflip(x : Int16, p : Nat) : Int16`

Returns the value of flipping bit `p mod 16` in `x`.

## Value `bitcountNonZero`
`let bitcountNonZero : (x : Int16) -> Int16`

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
`let bitcountLeadingZero : (x : Int16) -> Int16`

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
`let bitcountTrailingZero : (x : Int16) -> Int16`

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
`func addWrap(x : Int16, y : Int16) : Int16`

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
`func subWrap(x : Int16, y : Int16) : Int16`

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
`func mulWrap(x : Int16, y : Int16) : Int16`

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
`func powWrap(x : Int16, y : Int16) : Int16`

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
