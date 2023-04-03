# Int16
16-bit signed integers with checked arithmetic.

Common 16-bit integer functions.
Most operations are available as built-in operators (e.g. `1 + 1`).

## Type `Int16`
``` motoko no-repl
type Int16 = Prim.Types.Int16
```

16-bit signed integers.

## Value `minimumValue`
``` motoko no-repl
let minimumValue : Int16
```

Minimum 16-bit integer value, `-2 ** 15`.

## Value `maximumValue`
``` motoko no-repl
let maximumValue : Int16
```

Maximum 16-bit integer value, `+2 ** 15 - 1`.

## Value `toInt`
``` motoko no-repl
let toInt : Int16 -> Int
```

Converts a 16-bit signed integer to a signed integer with infinite precision.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.toInt(12_345) // => 12_345 : Int
```

## Value `fromInt`
``` motoko no-repl
let fromInt : Int -> Int16
```

Converts a signed integer with infinite precision to a 16-bit signed integer.

Traps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.fromInt(12_345) // => +12_345 : Int16
```

## Value `fromIntWrap`
``` motoko no-repl
let fromIntWrap : Int -> Int16
```

Converts a signed integer with infinite precision to a 16-bit signed integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.fromIntWrap(-12_345) // => -12_345 : Int
```

## Value `fromNat16`
``` motoko no-repl
let fromNat16 : Nat16 -> Int16
```

Converts an unsigned 16-bit integer to a signed 16-bit integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.fromNat16(12_345) // => +12_345 : Int16
```

## Value `toNat16`
``` motoko no-repl
let toNat16 : Int16 -> Nat16
```

Converts a signed 16-bit integer to an unsigned 16-bit integer.

Wraps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.toNat16(-1) // => 65_535 : Nat16 // underflow
```

## Function `toText`
``` motoko no-repl
func toText(x : Int16) : Text
```

Returns the Text representation of `x`.
Formats the integer in decimal representation without underscore separators for thousand figures.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.toText(-12345) // => "-12345"
```

## Function `abs`
``` motoko no-repl
func abs(x : Int16) : Int16
```

Returns the absolute value of `x`.

Traps when `x == -2 ** 15` (the minimum `Int16` value).

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.abs(-12345) // => +12_345
```

## Function `min`
``` motoko no-repl
func min(x : Int16, y : Int16) : Int16
```

Returns the minimum of `x` and `y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.min(+2, -3) // => -3
```

## Function `max`
``` motoko no-repl
func max(x : Int16, y : Int16) : Int16
```

Returns the maximum of `x` and `y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.max(+2, -3) // => +2
```

## Function `equal`
``` motoko no-repl
func equal(x : Int16, y : Int16) : Bool
```

Returns `x == y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.equal(123, 123) // => true
```

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Int16, y : Int16) : Bool
```

Returns `x != y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.notEqual(123, 123) // => false
```

## Function `less`
``` motoko no-repl
func less(x : Int16, y : Int16) : Bool
```

Returns `x < y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.less(123, 1234) // => true
```

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Int16, y : Int16) : Bool
```

Returns `x <= y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.lessOrEqual(123, 1234) // => true
```

## Function `greater`
``` motoko no-repl
func greater(x : Int16, y : Int16) : Bool
```

Returns `x > y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.greater(1234, 123) // => true
```

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Int16, y : Int16) : Bool
```

Returns `x >= y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.greaterOrEqual(1234, 123) // => true
```

## Function `compare`
``` motoko no-repl
func compare(x : Int16, y : Int16) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.compare(123, 1234) // => #less
```

## Function `neg`
``` motoko no-repl
func neg(x : Int16) : Int16
```

Returns the negation of `x`, `-x`.

Traps on overflow, i.e. for `neg(-2 ** 15)`.


Example:
```motoko
import Int16 "mo:base/Int16";

Int16.neg(123) // => -123
```

## Function `add`
``` motoko no-repl
func add(x : Int16, y : Int16) : Int16
```

Returns the sum of `x` and `y`, `x + y`.

Traps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.add(1234, 123) // => +1_357
```

## Function `sub`
``` motoko no-repl
func sub(x : Int16, y : Int16) : Int16
```

Returns the difference of `x` and `y`, `x - y`.

Traps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.sub(1234, 123) // => +1_111
```

## Function `mul`
``` motoko no-repl
func mul(x : Int16, y : Int16) : Int16
```

Returns the product of `x` and `y`, `x * y`.

Traps on overflow/underflow.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.mul(123, 100) // => +12_300
```

## Function `div`
``` motoko no-repl
func div(x : Int16, y : Int16) : Int16
```

Returns the signed integer division of `x` by `y`, `x / y`.
Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.

Traps when `y` is zero.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.div(123, 10) // => +12
```

## Function `rem`
``` motoko no-repl
func rem(x : Int16, y : Int16) : Int16
```

Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
which is defined as `x - x / y * y`.

Traps when `y` is zero.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.rem(123, 10) // => +3
```

## Function `pow`
``` motoko no-repl
func pow(x : Int16, y : Int16) : Int16
```

Returns `x` to the power of `y`, `x ** y`.

Traps on overflow/underflow and when `y < 0 or y >= 16`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.pow(2, 10) // => +1_024
```

## Function `bitnot`
``` motoko no-repl
func bitnot(x : Int16) : Int16
```

Returns the bitwise negation of `x`, `^x`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitnot(-256 /* 0xff00 */) // => +255 // 0xff
```

## Function `bitand`
``` motoko no-repl
func bitand(x : Int16, y : Int16) : Int16
```

Returns the bitwise "and" of `x` and `y`, `x & y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitand(0x0fff, 0x00f0) // => +240 // 0xf0
```

## Function `bitor`
``` motoko no-repl
func bitor(x : Int16, y : Int16) : Int16
```

Returns the bitwise "or" of `x` and `y`, `x | y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitor(0x0f0f, 0x00f0) // => +4_095 // 0x0fff
```

## Function `bitxor`
``` motoko no-repl
func bitxor(x : Int16, y : Int16) : Int16
```

Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitxor(0x0fff, 0x00f0) // => +3_855 // 0x0f0f
```

## Function `bitshiftLeft`
``` motoko no-repl
func bitshiftLeft(x : Int16, y : Int16) : Int16
```

Returns the bitwise left shift of `x` by `y`, `x << y`.
The right bits of the shift filled with zeros.
Left-overflowing bits, including the sign bit, are discarded.

For `y >= 16`, the semantics is the same as for `bitshiftLeft(x, y % 16)`.
For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitshiftLeft(1, 8) // => +256 // 0x100 equivalent to `2 ** 8`.
```

## Function `bitshiftRight`
``` motoko no-repl
func bitshiftRight(x : Int16, y : Int16) : Int16
```

Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
The sign bit is retained and the left side is filled with the sign bit.
Right-underflowing bits are discarded, i.e. not rotated to the left side.

For `y >= 16`, the semantics is the same as for `bitshiftRight(x, y % 16)`.
For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitshiftRight(1024, 8) // => +4 // equivalent to `1024 / (2 ** 8)`
```

## Function `bitrotLeft`
``` motoko no-repl
func bitrotLeft(x : Int16, y : Int16) : Int16
```

Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
Each left-overflowing bit is inserted again on the right side.
The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.

Changes the direction of rotation for negative `y`.
For `y >= 16`, the semantics is the same as for `bitrotLeft(x, y % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitrotLeft(0x2001, 4) // => +18 // 0x12.
```

## Function `bitrotRight`
``` motoko no-repl
func bitrotRight(x : Int16, y : Int16) : Int16
```

Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
Each right-underflowing bit is inserted again on the right side.
The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.

Changes the direction of rotation for negative `y`.
For `y >= 16`, the semantics is the same as for `bitrotRight(x, y % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitrotRight(0x2010, 8) // => +4_128 // 0x01020.
```

## Function `bittest`
``` motoko no-repl
func bittest(x : Int16, p : Nat) : Bool
```

Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
If `p >= 16`, the semantics is the same as for `bittest(x, p % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bittest(128, 7) // => true
```

## Function `bitset`
``` motoko no-repl
func bitset(x : Int16, p : Nat) : Int16
```

Returns the value of setting bit `p` in `x` to `1`.
If `p >= 16`, the semantics is the same as for `bitset(x, p % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitset(0, 7) // => +128
```

## Function `bitclear`
``` motoko no-repl
func bitclear(x : Int16, p : Nat) : Int16
```

Returns the value of clearing bit `p` in `x` to `0`.
If `p >= 16`, the semantics is the same as for `bitclear(x, p % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitclear(-1, 7) // => -129
```

## Function `bitflip`
``` motoko no-repl
func bitflip(x : Int16, p : Nat) : Int16
```

Returns the value of flipping bit `p` in `x`.
If `p >= 16`, the semantics is the same as for `bitclear(x, p % 16)`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitflip(255, 7) // => +127
```

## Value `bitcountNonZero`
``` motoko no-repl
let bitcountNonZero : (x : Int16) -> Int16
```

Returns the count of non-zero bits in `x`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitcountNonZero(0xff) // => +8
```

## Value `bitcountLeadingZero`
``` motoko no-repl
let bitcountLeadingZero : (x : Int16) -> Int16
```

Returns the count of leading zero bits in `x`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitcountLeadingZero(0x80) // => +8
```

## Value `bitcountTrailingZero`
``` motoko no-repl
let bitcountTrailingZero : (x : Int16) -> Int16
```

Returns the count of trailing zero bits in `x`.

Example:
```motoko
import Int16 "mo:base/Int16";

Int16.bitcountTrailingZero(0x0100) // => +8
```

## Function `addWrap`
``` motoko no-repl
func addWrap(x : Int16, y : Int16) : Int16
```

Returns the sum of `x` and `y`, `x +% y`.

Wraps on overflow/underflow.


Example:
```motoko
import Int16 "mo:base/Int16";

Int16.addWrap(2 ** 14, 2 ** 14) // => -32_768 // overflow
```

## Function `subWrap`
``` motoko no-repl
func subWrap(x : Int16, y : Int16) : Int16
```

Returns the difference of `x` and `y`, `x -% y`.

Wraps on overflow/underflow.


Example:
```motoko
import Int16 "mo:base/Int16";

Int16.subWrap(-2 ** 15, 1) // => +32_767 // underflow
```

## Function `mulWrap`
``` motoko no-repl
func mulWrap(x : Int16, y : Int16) : Int16
```

Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.

Wraps on overflow/underflow.


Example:
```motoko
import Int16 "mo:base/Int16";

Int16.mulWrap(2 ** 8, 2 ** 8) // => 0 // overflow
```

## Function `powWrap`
``` motoko no-repl
func powWrap(x : Int16, y : Int16) : Int16
```

Returns `x` to the power of `y`, `x **% y`.

Wraps on overflow/underflow.
Traps if `y < 0 or y >= 16`.


Example:
```motoko
import Int16 "mo:base/Int16";

Int16.powWrap(2, 15) // => -32_768 // overflow
```
