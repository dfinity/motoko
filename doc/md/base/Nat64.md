# Nat64
64-bit unsigned integers with checked arithmetic

Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Nat64`
``` motoko no-repl
type Nat64 = Prim.Types.Nat64
```

64-bit natural numbers.

## Value `toNat`
``` motoko no-repl
let toNat : Nat64 -> Nat
```

Conversion.

## Value `fromNat`
``` motoko no-repl
let fromNat : Nat -> Nat64
```

Conversion. Traps on overflow/underflow.

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Nat64
```

Conversion. Wraps on overflow/underflow.

## Function `toText`
``` motoko no-repl
func toText(x : Nat64) : Text
```

Returns the Text representation of `x`.

## Function `min`
``` motoko no-repl
func min(x : Nat64, y : Nat64) : Nat64
```

Returns the minimum of `x` and `y`.

## Function `max`
``` motoko no-repl
func max(x : Nat64, y : Nat64) : Nat64
```

Returns the maximum of `x` and `y`.

## Function `equal`
``` motoko no-repl
func equal(x : Nat64, y : Nat64) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Nat64, y : Nat64) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko no-repl
func less(x : Nat64, y : Nat64) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Nat64, y : Nat64) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko no-repl
func greater(x : Nat64, y : Nat64) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Nat64, y : Nat64) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko no-repl
func compare(x : Nat64, y : Nat64) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `add`
``` motoko no-repl
func add(x : Nat64, y : Nat64) : Nat64
```

Returns the sum of `x` and `y`, `x + y`. Traps on overflow.

## Function `sub`
``` motoko no-repl
func sub(x : Nat64, y : Nat64) : Nat64
```

Returns the difference of `x` and `y`, `x - y`. Traps on underflow.

## Function `mul`
``` motoko no-repl
func mul(x : Nat64, y : Nat64) : Nat64
```

Returns the product of `x` and `y`, `x * y`. Traps on overflow.

## Function `div`
``` motoko no-repl
func div(x : Nat64, y : Nat64) : Nat64
```

Returns the division of `x by y`, `x / y`.
Traps when `y` is zero.

## Function `rem`
``` motoko no-repl
func rem(x : Nat64, y : Nat64) : Nat64
```

Returns the remainder of `x` divided by `y`, `x % y`.
Traps when `y` is zero.

## Function `pow`
``` motoko no-repl
func pow(x : Nat64, y : Nat64) : Nat64
```

Returns `x` to the power of `y`, `x ** y`. Traps on overflow.

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise negation of `x`, `^x`.

## Function `bitand`
``` motoko no-repl
func bitand(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise and of `x` and `y`, `x & y`.

## Function `bitor`
``` motoko no-repl
func bitor(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise or of `x` and `y`, `x \| y`.

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise shift left of `x` by `y`, `x << y`.

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise shift right of `x` by `y`, `x >> y`.

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise rotate left of `x` by `y`, `x <<> y`.

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Nat64, y : Nat64) : Nat64
```

Returns the bitwise rotate right of `x` by `y`, `x <>> y`.

## Function `bittest`
``` motoko no-repl
func bittest(x : Nat64, p : Nat) : Bool
```

Returns the value of bit `p mod 64` in `x`, `(x & 2^(p mod 64)) == 2^(p mod 64)`.

## Function `bitset`
``` motoko no-repl
func bitset(x : Nat64, p : Nat) : Nat64
```

Returns the value of setting bit `p mod 64` in `x` to `1`.

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Nat64, p : Nat) : Nat64
```

Returns the value of clearing bit `p mod 64` in `x` to `0`.

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Nat64, p : Nat) : Nat64
```

Returns the value of flipping bit `p mod 64` in `x`.

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Nat64) -> Nat64
```

Returns the count of non-zero bits in `x`.

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Nat64) -> Nat64
```

Returns the count of leading zero bits in `x`.

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Nat64) -> Nat64
```

Returns the count of trailing zero bits in `x`.

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Nat64, y : Nat64) : Nat64
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Nat64, y : Nat64) : Nat64
```

Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
