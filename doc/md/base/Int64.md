# Int64
64-bit signed integers with checked arithmetic.

Common 64-bit integer functions.
Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int64`
``` motoko no-repl
type Int64 = Prim.Types.Int64
```

64-bit signed integers.

## Value `minimumValue`
``` motoko no-repl
let minimumValue : Int64
```

Minimum 64-bit integer value, `-2 ** 63`.

## Value `maximumValue`
``` motoko no-repl
let maximumValue : Int64
```

Maximum 64-bit integer value, `+2 ** 63 - 1`.

## Value `toInt`
``` motoko no-repl
let toInt : Int64 -> Int
```

Converts a 64-bit signed integer to a signed integer with infinite precision.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.toInt(123_456) // => 123_456 : Int
```

## Value `fromInt`
``` motoko no-repl
let fromInt : Int -> Int64
```

Converts a signed integer with infinite precision to a 64-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.fromInt(123_456) // => +123_456 : Int64
```

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Int64
```

Converts a signed integer with infinite precision to a 64-bit signed integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.fromIntWrap(-123_456) // => -123_456 : Int64
```

## Value `fromNat64`
``` motoko no-repl
let fromNat64 : Nat64 -> Int64
```

Converts an unsigned 64-bit integer to a signed 64-bit integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.fromNat64(123_456) // => +123_456 : Int64
```

## Value `toNat64`
``` motoko no-repl
let toNat64 : Int64 -> Nat64
```

Converts a signed 64-bit integer to an unsigned 64-bit integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.toNat64(-1) // => 18_446_744_073_709_551_615 : Nat64 // underflow
```

## Function `toText`
``` motoko no-repl
func toText(x : Int64) : Text
```

Returns the Text representation of `x`.
Formats the integer in decimal representation without underscore separators for thousand figures.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.toText(-123456) // => "-123456"
```

## Function `abs`
``` motoko no-repl
func abs(x : Int64) : Int64
```

Returns the absolute value of `x`.

Traps when `x == -2 ** 63` (the minimum `Int64` value).

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.abs(-123456) // => +123_456
```

## Function `min`
``` motoko no-repl
func min(x : Int64, y : Int64) : Int64
```

Returns the minimum of `x` and `y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.min(+2, -3) // => -3
```

## Function `max`
``` motoko no-repl
func max(x : Int64, y : Int64) : Int64
```

Returns the maximum of `x` and `y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.max(+2, -3) // => +2
```

## Function `equal`
``` motoko no-repl
func equal(x : Int64, y : Int64) : Bool
```

Returns `x == y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.equal(123, 123) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int64, y : Int64) : Bool
```

Returns `x != y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.notEqual(123, 123) // => false
```

## Function `less`
``` motoko no-repl
func less(x : Int64, y : Int64) : Bool
```

Returns `x < y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.less(123, 1234) // => true
```

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Int64, y : Int64) : Bool
```

Returns `x <= y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.lessOrEqual(123, 1234) // => true
```

## Function `greater`
``` motoko no-repl
func greater(x : Int64, y : Int64) : Bool
```

Returns `x > y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.greater(1234, 123) // => true
```

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Int64, y : Int64) : Bool
```

Returns `x >= y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.greaterOrEqual(1234, 123) // => true
```

## Function `compare`
``` motoko no-repl
func compare(x : Int64, y : Int64) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.compare(123, 1234) // => #less
```

## Function `neg`
``` motoko no-repl
func neg(x : Int64) : Int64
```

Returns the negation of `x`, `-x`.

Traps on overflow, i.e. for `neg(-2 ** 63)`.


Example:
```motoko
import Int64 "mo:base/Int64";

Int64.neg(123) // => -123
```

## Function `add`
``` motoko no-repl
func add(x : Int64, y : Int64) : Int64
```

Returns the sum of `x` and `y`, `x + y`.

Traps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.add(1234, 123) // => +1_357
```

## Function `sub`
``` motoko no-repl
func sub(x : Int64, y : Int64) : Int64
```

Returns the difference of `x` and `y`, `x - y`.

Traps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.sub(1234, 123) // => +1_111
```

## Function `mul`
``` motoko no-repl
func mul(x : Int64, y : Int64) : Int64
```

Returns the product of `x` and `y`, `x * y`.

Traps on overflow/underflow.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.mul(123, 100) // => +12_300
```

## Function `div`
``` motoko no-repl
func div(x : Int64, y : Int64) : Int64
```

Returns the signed integer division of `x` by `y`, `x / y`.
Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.

Traps when `y` is zero.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.div(123, 10) // => +12
```

## Function `rem`
``` motoko no-repl
func rem(x : Int64, y : Int64) : Int64
```

Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
which is defined as `x - x / y * y`.

Traps when `y` is zero.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.rem(123, 10) // => +3
```

## Function `pow`
``` motoko no-repl
func pow(x : Int64, y : Int64) : Int64
```

Returns `x` to the power of `y`, `x ** y`.

Traps on overflow/underflow and when `y < 0 or y >= 64`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.pow(2, 10) // => +1_024
```

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Int64) : Int64
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitnot(-256 /* 0xffff_ffff_ffff_ff00 */) // => +255 // 0xff
```

## Function `bitand`
``` motoko no-repl
func bitand(x : Int64, y : Int64) : Int64
```

Returns the bitwise "and" of `x` and `y`, `x & y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitand(0xffff, 0x00f0) // => +240 // 0xf0
```

## Function `bitor`
``` motoko no-repl
func bitor(x : Int64, y : Int64) : Int64
```

Returns the bitwise "or" of `x` and `y`, `x | y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitor(0xffff, 0x00f0) // => +65_535 // 0xffff
```

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Int64, y : Int64) : Int64
```

Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitxor(0xffff, 0x00f0) // => +65_295 // 0xff0f
```

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Int64, y : Int64) : Int64
```

Returns the bitwise left shift of `x` by `y`, `x << y`.
The right bits of the shift filled with zeros.
Left-overflowing bits, including the sign bit, are discarded.

For `y >= 64`, the semantics is the same as for `bitshiftLeft(x, y % 64)`.
For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitshiftLeft(1, 8) // => +256 // 0x100 equivalent to `2 ** 8`.
```

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Int64, y : Int64) : Int64
```

Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
The sign bit is retained and the left side is filled with the sign bit.
Right-underflowing bits are discarded, i.e. not rotated to the left side.

For `y >= 64`, the semantics is the same as for `bitshiftRight(x, y % 64)`.
For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitshiftRight(1024, 8) // => +4 // equivalent to `1024 / (2 ** 8)`
```

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Int64, y : Int64) : Int64
```

Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
Each left-overflowing bit is inserted again on the right side.
The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.

Changes the direction of rotation for negative `y`.
For `y >= 64`, the semantics is the same as for `bitrotLeft(x, y % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitrotLeft(0x2000_0000_0000_0001, 4) // => +18 // 0x12.
```

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Int64, y : Int64) : Int64
```

Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
Each right-underflowing bit is inserted again on the right side.
The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.

Changes the direction of rotation for negative `y`.
For `y >= 64`, the semantics is the same as for `bitrotRight(x, y % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitrotRight(0x0002_0000_0000_0001, 48) // => +65538 // 0x1_0002.
```

## Function `bittest`
``` motoko no-repl
func bittest(x : Int64, p : Nat) : Bool
```

Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
If `p >= 64`, the semantics is the same as for `bittest(x, p % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bittest(128, 7) // => true
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Int64, p : Nat) : Int64
```

Returns the value of setting bit `p` in `x` to `1`.
If `p >= 64`, the semantics is the same as for `bitset(x, p % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitset(0, 7) // => +128
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Int64, p : Nat) : Int64
```

Returns the value of clearing bit `p` in `x` to `0`.
If `p >= 64`, the semantics is the same as for `bitclear(x, p % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitclear(-1, 7) // => -129
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Int64, p : Nat) : Int64
```

Returns the value of flipping bit `p` in `x`.
If `p >= 64`, the semantics is the same as for `bitclear(x, p % 64)`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitflip(255, 7) // => +127
```

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Int64) -> Int64
```

Returns the count of non-zero bits in `x`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitcountNonZero(0xffff) // => +16
```

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Int64) -> Int64
```

Returns the count of leading zero bits in `x`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitcountLeadingZero(0x8000_0000) // => +32
```

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Int64) -> Int64
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko
import Int64 "mo:base/Int64";

Int64.bitcountTrailingZero(0x0201_0000) // => +16
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Int64, y : Int64) : Int64
```

Returns the sum of `x` and `y`, `x +% y`.

Wraps on overflow/underflow.


Example:
```motoko
import Int64 "mo:base/Int64";

Int64.addWrap(2 ** 62, 2 ** 62) // => -9_223_372_036_854_775_808 // overflow
```

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Int64, y : Int64) : Int64
```

Returns the difference of `x` and `y`, `x -% y`.

Wraps on overflow/underflow.


Example:
```motoko
import Int64 "mo:base/Int64";

Int64.subWrap(-2 ** 63, 1) // => +9_223_372_036_854_775_807 // underflow
```

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Int64, y : Int64) : Int64
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Wraps on overflow/underflow.


Example:
```motoko
import Int64 "mo:base/Int64";

Int64.mulWrap(2 ** 32, 2 ** 32) // => 0 // overflow
```

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Int64, y : Int64) : Int64
```

Returns `x` to the power of `y`, `x **% y`.

Wraps on overflow/underflow.
Traps if `y < 0 or y >= 64`.


Example:
```motoko
import Int64 "mo:base/Int64";

Int64.powWrap(2, 63) // => -9_223_372_036_854_775_808 // overflow
```
