# Int64
64-bit signed integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int64`
``` motoko norepl
type Int64 = Prim.Types.Int64
```

64-bit signed integers.

## Value `toInt`
``` motoko norepl
let toInt : Int64 -> Int
```

Conversion.

## Value `fromInt`
``` motoko norepl
let fromInt : Int -> Int64
```

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
``` motoko norepl
let fromIntWrap : Int -> Int64
```

Conversion. Wraps on overflow/underflow.

## Value `fromNat64`
``` motoko norepl
let fromNat64 : Nat64 -> Int64
```

Conversion. Wraps on overflow/underflow.

## Value `toNat64`
``` motoko norepl
let toNat64 : Int64 -> Nat64
```

Conversion. Wraps on overflow/underflow.

## Function `toText`
``` motoko norepl
func toText(x : Int64) : Text
```

Returns the Text representation of `x`.

## Function `abs`
``` motoko norepl
func abs(x : Int64) : Int64
```

Returns the absolute value of `x`. Traps when `x = -2^63`.

## Function `min`
``` motoko norepl
func min(x : Int64, y : Int64) : Int64
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko norepl
func max(x : Int64, y : Int64) : Int64
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko norepl
func equal(x : Int64, y : Int64) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Int64, y : Int64) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Int64, y : Int64) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Int64, y : Int64) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Int64, y : Int64) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Int64, y : Int64) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Int64, y : Int64) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `neg`
``` motoko norepl
func neg(x : Int64) : Int64
```

Returns the negation of `x`, `-x`. Traps on overflow.

## Function `add`
``` motoko norepl
func add(x : Int64, y : Int64) : Int64
```

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
``` motoko norepl
func sub(x : Int64, y : Int64) : Int64
```

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
``` motoko norepl
func mul(x : Int64, y : Int64) : Int64
```

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
``` motoko norepl
func div(x : Int64, y : Int64) : Int64
```

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko norepl
func rem(x : Int64, y : Int64) : Int64
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko norepl
func pow(x : Int64, y : Int64) : Int64
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
``` motoko norepl
func bitnot(x : Int64, y : Int64) : Int64
```

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
``` motoko norepl
func bitand(x : Int64, y : Int64) : Int64
```

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
``` motoko norepl
func bitor(x : Int64, y : Int64) : Int64
```

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
``` motoko norepl
func bitxor(x : Int64, y : Int64) : Int64
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
``` motoko norepl
func bitshiftLeft(x : Int64, y : Int64) : Int64
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
``` motoko norepl
func bitshiftRight(x : Int64, y : Int64) : Int64
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
``` motoko norepl
func bitrotLeft(x : Int64, y : Int64) : Int64
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
``` motoko norepl
func bitrotRight(x : Int64, y : Int64) : Int64
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
``` motoko norepl
func bittest(x : Int64, p : Nat) : Bool
```

Returns the value of bit `p mod 64` in `x`, `(x & 2^(p mod 64)) == 2^(p mod 64)`.

## Function `bitset`
``` motoko norepl
func bitset(x : Int64, p : Nat) : Int64
```

Returns the value of setting bit `p mod 64` in `x` to `1`.

## Function `bitclear`
``` motoko norepl
func bitclear(x : Int64, p : Nat) : Int64
```

Returns the value of clearing bit `p mod 64` in `x` to `0`.

## Function `bitflip`
``` motoko norepl
func bitflip(x : Int64, p : Nat) : Int64
```

Returns the value of flipping bit `p mod 64` in `x`.

## Value `bitcountNonZero`
``` motoko norepl
let bitcountNonZero : (x : Int64) -> Int64
```

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
``` motoko norepl
let bitcountLeadingZero : (x : Int64) -> Int64
```

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
``` motoko norepl
let bitcountTrailingZero : (x : Int64) -> Int64
```

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
``` motoko norepl
func addWrap(x : Int64, y : Int64) : Int64
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
``` motoko norepl
func subWrap(x : Int64, y : Int64) : Int64
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
``` motoko norepl
func mulWrap(x : Int64, y : Int64) : Int64
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
``` motoko norepl
func powWrap(x : Int64, y : Int64) : Int64
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
