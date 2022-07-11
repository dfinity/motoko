# Nat16
16-bit unsigned integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Nat16`
``` motoko norepl
type Nat16 = Prim.Types.Nat16
```

16-bit natural numbers.

## Value `toNat`
``` motoko norepl
let toNat : Nat16 -> Nat
```

Conversion.

## Value `fromNat`
``` motoko norepl
let fromNat : Nat -> Nat16
```

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
``` motoko norepl
let fromIntWrap : Int -> Nat16
```

Conversion. Wraps on overflow/underflow.

## Function `toText`
``` motoko norepl
func toText(x : Nat16) : Text
```

Returns the Text representation of `x`.

## Function `min`
``` motoko norepl
func min(x : Nat16, y : Nat16) : Nat16
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko norepl
func max(x : Nat16, y : Nat16) : Nat16
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko norepl
func equal(x : Nat16, y : Nat16) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Nat16, y : Nat16) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Nat16, y : Nat16) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Nat16, y : Nat16) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Nat16, y : Nat16) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Nat16, y : Nat16) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Nat16, y : Nat16) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `add`
``` motoko norepl
func add(x : Nat16, y : Nat16) : Nat16
```

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
``` motoko norepl
func sub(x : Nat16, y : Nat16) : Nat16
```

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
``` motoko norepl
func mul(x : Nat16, y : Nat16) : Nat16
```

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
``` motoko norepl
func div(x : Nat16, y : Nat16) : Nat16
```

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko norepl
func rem(x : Nat16, y : Nat16) : Nat16
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko norepl
func pow(x : Nat16, y : Nat16) : Nat16
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
``` motoko norepl
func bitnot(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
``` motoko norepl
func bitand(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
``` motoko norepl
func bitor(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
``` motoko norepl
func bitxor(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
``` motoko norepl
func bitshiftLeft(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
``` motoko norepl
func bitshiftRight(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
``` motoko norepl
func bitrotLeft(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
``` motoko norepl
func bitrotRight(x : Nat16, y : Nat16) : Nat16
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
``` motoko norepl
func bittest(x : Nat16, p : Nat) : Bool
```

Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.

## Function `bitset`
``` motoko norepl
func bitset(x : Nat16, p : Nat) : Nat16
```

Returns the value of setting bit `p mod 16` in `x` to `1`.

## Function `bitclear`
``` motoko norepl
func bitclear(x : Nat16, p : Nat) : Nat16
```

Returns the value of clearing bit `p mod 16` in `x` to `0`.

## Function `bitflip`
``` motoko norepl
func bitflip(x : Nat16, p : Nat) : Nat16
```

Returns the value of flipping bit `p mod 16` in `x`.

## Value `bitcountNonZero`
``` motoko norepl
let bitcountNonZero : (x : Nat16) -> Nat16
```

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
``` motoko norepl
let bitcountLeadingZero : (x : Nat16) -> Nat16
```

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
``` motoko norepl
let bitcountTrailingZero : (x : Nat16) -> Nat16
```

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
``` motoko norepl
func addWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
``` motoko norepl
func subWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
``` motoko norepl
func mulWrap(x : Nat16, y : Nat16) : Nat16
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
``` motoko norepl
func powWrap(x : Nat16, y : Nat16) : Nat16
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
