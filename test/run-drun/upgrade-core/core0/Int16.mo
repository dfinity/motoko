/// Utility functions on 16-bit signed integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Int16 "mo:core/Int16";
/// ```

import Int "Int";
import Iter "Iter";
import Prim "mo:⛔";
import Order "Order";

module {
  /// 16-bit signed integers.
  public type Int16 = Prim.Types.Int16;

  /// Minimum 16-bit integer value, `-2 ** 15`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.minValue == (-32_768 : Int16);
  /// ```
  public let minValue : Int16 = -32_768;

  /// Maximum 16-bit integer value, `+2 ** 15 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.maxValue == (+32_767 : Int16);
  /// ```
  public let maxValue : Int16 = 32_767;

  /// Converts a 16-bit signed integer to a signed integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.toInt(12_345) == (12_345 : Int);
  /// ```
  public let toInt : Int16 -> Int = Prim.int16ToInt;

  /// Converts a signed integer with infinite precision to a 16-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.fromInt(12_345) == (+12_345 : Int16);
  /// ```
  public let fromInt : Int -> Int16 = Prim.intToInt16;

  /// Converts a signed integer with infinite precision to a 16-bit signed integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.fromIntWrap(-12_345) == (-12_345 : Int);
  /// ```
  public let fromIntWrap : Int -> Int16 = Prim.intToInt16Wrap;

  /// Converts a 8-bit signed integer to a 16-bit signed integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.fromInt8(-123) == (-123 : Int16);
  /// ```
  public let fromInt8 : Int8 -> Int16 = Prim.int8ToInt16;

  /// Converts a 16-bit signed integer to a 8-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.toInt8(-123) == (-123 : Int8);
  /// ```
  public let toInt8 : Int16 -> Int8 = Prim.int16ToInt8;

  /// Converts a 32-bit signed integer to a 16-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.fromInt32(-12_345) == (-12_345 : Int16);
  /// ```
  public let fromInt32 : Int32 -> Int16 = Prim.int32ToInt16;

  /// Converts a 16-bit signed integer to a 32-bit signed integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.toInt32(-12_345) == (-12_345 : Int32);
  /// ```
  public let toInt32 : Int16 -> Int32 = Prim.int16ToInt32;

  /// Converts an unsigned 16-bit integer to a signed 16-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.fromNat16(12_345) == (+12_345 : Int16);
  /// ```
  public let fromNat16 : Nat16 -> Int16 = Prim.nat16ToInt16;

  /// Converts a signed 16-bit integer to an unsigned 16-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.toNat16(-1) == (65_535 : Nat16); // underflow
  /// ```
  public let toNat16 : Int16 -> Nat16 = Prim.int16ToNat16;

  /// Returns the Text representation of `x`. Textual representation _do not_
  /// contain underscores to represent commas.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.toText(-12345) == "-12345";
  /// ```
  public func toText(x : Int16) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`.
  ///
  /// Traps when `x == -2 ** 15` (the minimum `Int16` value).
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.abs(-12345) == +12_345;
  /// ```
  public func abs(x : Int16) : Int16 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.min(+2, -3) == -3;
  /// ```
  public func min(x : Int16, y : Int16) : Int16 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.max(+2, -3) == +2;
  /// ```
  public func max(x : Int16, y : Int16) : Int16 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Int16 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.equal(-1, -1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// let a : Int16 = -123;
  /// let b : Int16 = 123;
  /// assert not Int16.equal(a, b);
  /// ```
  public func equal(x : Int16, y : Int16) : Bool { x == y };

  /// Inequality function for Int16 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.notEqual(-1, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Int16, y : Int16) : Bool { x != y };

  /// "Less than" function for Int16 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.less(-2, 1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Int16, y : Int16) : Bool { x < y };

  /// "Less than or equal" function for Int16 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.lessOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Int16, y : Int16) : Bool { x <= y };

  /// "Greater than" function for Int16 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert not Int16.greater(-2, 1);
  /// ```
  public func greater(x : Int16, y : Int16) : Bool { x > y };

  /// "Greater than or equal" function for Int16 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.greaterOrEqual(-2, -2);
  /// ```
  public func greaterOrEqual(x : Int16, y : Int16) : Bool { x >= y };

  /// General-purpose comparison function for `Int16`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.compare(-3, 2) == #less;
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.sort([1, -2, -3] : [Int16], Int16.compare) == [-3, -2, 1];
  /// ```
  public persistent func compare(x : Int16, y : Int16) : Order.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the negation of `x`, `-x`.
  ///
  /// Traps on overflow, i.e. for `neg(-2 ** 15)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.neg(123) == -123;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  public func neg(x : Int16) : Int16 { -x };

  /// Returns the sum of `x` and `y`, `x + y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.add(100, 23) == +123;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.foldLeft<Int16, Int16>([1, -2, -3], 0, Int16.add) == -4;
  /// ```
  public func add(x : Int16, y : Int16) : Int16 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.sub(123, 100) == +23;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.foldLeft<Int16, Int16>([1, -2, -3], 0, Int16.sub) == 4;
  /// ```
  public func sub(x : Int16, y : Int16) : Int16 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.mul(12, 10) == +120;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.foldLeft<Int16, Int16>([1, -2, -3], 1, Int16.mul) == 6;
  /// ```
  public func mul(x : Int16, y : Int16) : Int16 { x * y };

  /// Returns the signed integer division of `x` by `y`, `x / y`.
  /// Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.div(123, 10) == +12;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Int16, y : Int16) : Int16 { x / y };

  /// Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
  /// which is defined as `x - x / y * y`.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.rem(123, 10) == +3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Int16, y : Int16) : Int16 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`.
  ///
  /// Traps on overflow/underflow and when `y < 0 or y >= 16`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.pow(2, 10) == +1_024;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Int16, y : Int16) : Int16 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitnot(-256 /* 0xff00 */) == +255 // 0xff;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Int16) : Int16 { ^x };

  /// Returns the bitwise "and" of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitand(0x0fff, 0x00f0) == +240 // 0xf0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Int16, y : Int16) : Int16 { x & y };

  /// Returns the bitwise "or" of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitor(0x0f0f, 0x00f0) == +4_095 // 0x0fff;
  /// ```
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Int16, y : Int16) : Int16 { x | y };

  /// Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitxor(0x0fff, 0x00f0) == +3_855 // 0x0f0f;
  /// ```
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Int16, y : Int16) : Int16 { x ^ y };

  /// Returns the bitwise left shift of `x` by `y`, `x << y`.
  /// The right bits of the shift filled with zeros.
  /// Left-overflowing bits, including the sign bit, are discarded.
  ///
  /// For `y >= 16`, the semantics is the same as for `bitshiftLeft(x, y % 16)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitshiftLeft(1, 8) == +256 // 0x100 equivalent to `2 ** 8`.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Int16, y : Int16) : Int16 { x << y };

  /// Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
  /// The sign bit is retained and the left side is filled with the sign bit.
  /// Right-underflowing bits are discarded, i.e. not rotated to the left side.
  ///
  /// For `y >= 16`, the semantics is the same as for `bitshiftRight(x, y % 16)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitshiftRight(1024, 8) == +4 // equivalent to `1024 / (2 ** 8)`;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Int16, y : Int16) : Int16 { x >> y };

  /// Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
  /// Each left-overflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 16`, the semantics is the same as for `bitrotLeft(x, y % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitrotLeft(0x2001, 4) == +18 // 0x12.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Int16, y : Int16) : Int16 { x <<> y };

  /// Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
  /// Each right-underflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 16`, the semantics is the same as for `bitrotRight(x, y % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitrotRight(0x2010, 8) == +4_128 // 0x01020.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Int16, y : Int16) : Int16 { x <>> y };

  /// Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
  /// If `p >= 16`, the semantics is the same as for `bittest(x, p % 16)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bittest(128, 7);
  /// ```
  public func bittest(x : Int16, p : Nat) : Bool {
    Prim.btstInt16(x, Prim.intToInt16(p))
  };

  /// Returns the value of setting bit `p` in `x` to `1`.
  /// If `p >= 16`, the semantics is the same as for `bitset(x, p % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitset(0, 7) == +128;
  /// ```
  public func bitset(x : Int16, p : Nat) : Int16 {
    x | (1 << Prim.intToInt16(p))
  };

  /// Returns the value of clearing bit `p` in `x` to `0`.
  /// If `p >= 16`, the semantics is the same as for `bitclear(x, p % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitclear(-1, 7) == -129;
  /// ```
  public func bitclear(x : Int16, p : Nat) : Int16 {
    x & ^(1 << Prim.intToInt16(p))
  };

  /// Returns the value of flipping bit `p` in `x`.
  /// If `p >= 16`, the semantics is the same as for `bitclear(x, p % 16)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitflip(255, 7) == +127;
  /// ```
  public func bitflip(x : Int16, p : Nat) : Int16 {
    x ^ (1 << Prim.intToInt16(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitcountNonZero(0xff) == +8;
  /// ```
  public let bitcountNonZero : (x : Int16) -> Int16 = Prim.popcntInt16;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitcountLeadingZero(0x80) == +8;
  /// ```
  public let bitcountLeadingZero : (x : Int16) -> Int16 = Prim.clzInt16;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.bitcountTrailingZero(0x0100) == +8;
  /// ```
  public let bitcountTrailingZero : (x : Int16) -> Int16 = Prim.ctzInt16;

  /// Returns the upper (i.e. most significant) and lower (least significant) byte of `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.explode 0x77ee == (119, 238);
  /// ```
  public let explode : (x : Int16) -> (msb : Nat8, lsb : Nat8) = Prim.explodeInt16;

  /// Returns the sum of `x` and `y`, `x +% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.addWrap(2 ** 14, 2 ** 14) == -32_768; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Int16, y : Int16) : Int16 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.subWrap(-2 ** 15, 1) == +32_767; // underflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Int16, y : Int16) : Int16 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int16.mulWrap(2 ** 8, 2 ** 8) == 0; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Int16, y : Int16) : Int16 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`.
  ///
  /// Wraps on overflow/underflow.
  /// Traps if `y < 0 or y >= 16`.
  ///
  /// Example:
  /// ```motoko include=import
  ///
  /// assert Int16.powWrap(2, 15) == -32_768; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Int16, y : Int16) : Int16 { x **% y };

  /// Returns an iterator over `Int16` values from the first to second argument with an exclusive upper bound.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int16.range(1, 4);
  /// assert iter.next() == ?1;
  /// assert iter.next() == ?2;
  /// assert iter.next() == ?3;
  /// assert iter.next() == null;
  /// ```
  ///
  /// If the first argument is greater than the second argument, the function returns an empty iterator.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int16.range(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func range(fromInclusive : Int16, toExclusive : Int16) : Iter.Iter<Int16> {
    if (fromInclusive >= toExclusive) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = fromInclusive;
        public func next() : ?Int16 {
          if (n == toExclusive) {
            null
          } else {
            let result = n;
            n += 1;
            ?result
          }
        }
      })
    }
  };

  /// Returns an iterator over `Int16` values from the first to second argument, inclusive.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int16.rangeInclusive(1, 3);
  /// assert iter.next() == ?1;
  /// assert iter.next() == ?2;
  /// assert iter.next() == ?3;
  /// assert iter.next() == null;
  /// ```
  ///
  /// If the first argument is greater than the second argument, the function returns an empty iterator.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int16.rangeInclusive(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func rangeInclusive(from : Int16, to : Int16) : Iter.Iter<Int16> {
    if (from > to) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = from;
        var done = false;
        public func next() : ?Int16 {
          if (done) {
            null
          } else {
            let result = n;
            if (n == to) {
              done := true
            } else {
              n += 1
            };
            ?result
          }
        }
      })
    }
  };

  /// Returns an iterator over all Int16 values, from minValue to maxValue.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int16.allValues();
  /// assert iter.next() == ?-32_768;
  /// assert iter.next() == ?-32_767;
  /// assert iter.next() == ?-32_766;
  /// // ...
  /// ```
  public func allValues() : Iter.Iter<Int16> {
    rangeInclusive(minValue, maxValue)
  };
}
