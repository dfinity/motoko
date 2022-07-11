# Int32
32-bit signed integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int32`
``` motoko norepl
type Int32 = Prim.Types.Int32
```

32-bit signed integers.

## Value `toInt`
``` motoko norepl
let toInt : Int32 -> Int
```

Conversion.

## Value `fromInt`
``` motoko norepl
let fromInt : Int -> Int32
```

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
``` motoko norepl
let fromIntWrap : Int -> Int32
```

Conversion. Wraps on overflow/underflow.

## Value `fromNat32`
``` motoko norepl
let fromNat32 : Nat32 -> Int32
```

Conversion. Wraps on overflow/underflow.

## Value `toNat32`
``` motoko norepl
let toNat32 : Int32 -> Nat32
```

Conversion. Wraps on overflow/underflow.

## Function `toText`
``` motoko norepl
func toText(x : Int32) : Text
```

Returns the Text representation of `x`.

## Function `abs`
``` motoko norepl
func abs(x : Int32) : Int32
```

Returns the absolute value of `x`. Traps when `x = -2^31`.

## Function `min`
``` motoko norepl
func min(x : Int32, y : Int32) : Int32
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko norepl
func max(x : Int32, y : Int32) : Int32
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko norepl
func equal(x : Int32, y : Int32) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Int32, y : Int32) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Int32, y : Int32) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Int32, y : Int32) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Int32, y : Int32) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Int32, y : Int32) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Int32, y : Int32) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `neg`
``` motoko norepl
func neg(x : Int32) : Int32
```

Returns the negation of `x`, `-x`. Traps on overflow.

## Function `add`
``` motoko norepl
func add(x : Int32, y : Int32) : Int32
```

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
``` motoko norepl
func sub(x : Int32, y : Int32) : Int32
```

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
``` motoko norepl
func mul(x : Int32, y : Int32) : Int32
```

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
``` motoko norepl
func div(x : Int32, y : Int32) : Int32
```

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko norepl
func rem(x : Int32, y : Int32) : Int32
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko norepl
func pow(x : Int32, y : Int32) : Int32
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
``` motoko norepl
func bitnot(x : Int32, y : Int32) : Int32
```

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
``` motoko norepl
func bitand(x : Int32, y : Int32) : Int32
```

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
``` motoko norepl
func bitor(x : Int32, y : Int32) : Int32
```

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
``` motoko norepl
func bitxor(x : Int32, y : Int32) : Int32
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
``` motoko norepl
func bitshiftLeft(x : Int32, y : Int32) : Int32
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
``` motoko norepl
func bitshiftRight(x : Int32, y : Int32) : Int32
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
``` motoko norepl
func bitrotLeft(x : Int32, y : Int32) : Int32
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
``` motoko norepl
func bitrotRight(x : Int32, y : Int32) : Int32
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
``` motoko norepl
func bittest(x : Int32, p : Nat) : Bool
```

Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.

## Function `bitset`
``` motoko norepl
func bitset(x : Int32, p : Nat) : Int32
```

Returns the value of setting bit `p mod 16` in `x` to `1`.

## Function `bitclear`
``` motoko norepl
func bitclear(x : Int32, p : Nat) : Int32
```

Returns the value of clearing bit `p mod 16` in `x` to `0`.

## Function `bitflip`
``` motoko norepl
func bitflip(x : Int32, p : Nat) : Int32
```

Returns the value of flipping bit `p mod 16` in `x`.

## Value `bitcountNonZero`
``` motoko norepl
let bitcountNonZero : (x : Int32) -> Int32
```

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
``` motoko norepl
let bitcountLeadingZero : (x : Int32) -> Int32
```

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
``` motoko norepl
let bitcountTrailingZero : (x : Int32) -> Int32
```

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
``` motoko norepl
func addWrap(x : Int32, y : Int32) : Int32
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
``` motoko norepl
func subWrap(x : Int32, y : Int32) : Int32
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
``` motoko norepl
func mulWrap(x : Int32, y : Int32) : Int32
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
``` motoko norepl
func powWrap(x : Int32, y : Int32) : Int32
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
