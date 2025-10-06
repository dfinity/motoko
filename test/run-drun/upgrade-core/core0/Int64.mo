/// Utility functions on 64-bit signed integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Int64 "mo:core/Int64";
/// ```

import Int "Int";
import Iter "Iter";
import Prim "mo:⛔";
import Order "Order";

module {

  /// 64-bit signed integers.
  public type Int64 = Prim.Types.Int64;

  /// Minimum 64-bit integer value, `-2 ** 63`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.minValue == -9_223_372_036_854_775_808;
  /// ```
  public let minValue : Int64 = -9_223_372_036_854_775_808;

  /// Maximum 64-bit integer value, `+2 ** 63 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.maxValue == +9_223_372_036_854_775_807;
  /// ```
  public let maxValue : Int64 = 9_223_372_036_854_775_807;

  /// Converts a 64-bit signed integer to a signed integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.toInt(123_456) == (123_456 : Int);
  /// ```
  public let toInt : Int64 -> Int = Prim.int64ToInt;

  /// Converts a signed integer with infinite precision to a 64-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.fromInt(123_456) == (+123_456 : Int64);
  /// ```
  public let fromInt : Int -> Int64 = Prim.intToInt64;

  /// Converts a 32-bit signed integer to a 64-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.fromInt32(-123_456) == (-123_456 : Int64);
  /// ```
  public let fromInt32 : Int32 -> Int64 = Prim.int32ToInt64;

  /// Converts a 64-bit signed integer to a 32-bit signed integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.toInt32(-123_456) == (-123_456 : Int32);
  /// ```
  public let toInt32 : Int64 -> Int32 = Prim.int64ToInt32;

  /// Converts a signed integer with infinite precision to a 64-bit signed integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.fromIntWrap(-123_456) == (-123_456 : Int64);
  /// ```
  public let fromIntWrap : Int -> Int64 = Prim.intToInt64Wrap;

  /// Converts an unsigned 64-bit integer to a signed 64-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.fromNat64(123_456) == (+123_456 : Int64);
  /// ```
  public let fromNat64 : Nat64 -> Int64 = Prim.nat64ToInt64;

  /// Converts a signed 64-bit integer to an unsigned 64-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.toNat64(-1) == (18_446_744_073_709_551_615 : Nat64); // underflow
  /// ```
  public let toNat64 : Int64 -> Nat64 = Prim.int64ToNat64;

  /// Returns the Text representation of `x`. Textual representation _do not_
  /// contain underscores to represent commas.
  ///
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.toText(-123456) == "-123456";
  /// ```
  public func toText(x : Int64) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`.
  ///
  /// Traps when `x == -2 ** 63` (the minimum `Int64` value).
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.abs(-123456) == +123_456;
  /// ```
  public func abs(x : Int64) : Int64 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.min(+2, -3) == -3;
  /// ```
  public func min(x : Int64, y : Int64) : Int64 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.max(+2, -3) == +2;
  /// ```
  public func max(x : Int64, y : Int64) : Int64 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Int64 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.equal(-1, -1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// let a : Int64 = -123;
  /// let b : Int64 = 123;
  /// assert not Int64.equal(a, b);
  /// ```
  public func equal(x : Int64, y : Int64) : Bool { x == y };

  /// Inequality function for Int64 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.notEqual(-1, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Int64, y : Int64) : Bool { x != y };

  /// "Less than" function for Int64 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.less(-2, 1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Int64, y : Int64) : Bool { x < y };

  /// "Less than or equal" function for Int64 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.lessOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Int64, y : Int64) : Bool { x <= y };

  /// "Greater than" function for Int64 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.greater(-2, -3);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>`
  /// as a function value at the moment.
  public func greater(x : Int64, y : Int64) : Bool { x > y };

  /// "Greater than or equal" function for Int64 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.greaterOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>=`
  /// as a function value at the moment.
  public func greaterOrEqual(x : Int64, y : Int64) : Bool { x >= y };

  /// General-purpose comparison function for `Int64`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.compare(-3, 2) == #less;
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.sort([1, -2, -3] : [Int64], Int64.compare) == [-3, -2, 1];
  /// ```
  public persistent func compare(x : Int64, y : Int64) : Order.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the negation of `x`, `-x`.
  ///
  /// Traps on overflow, i.e. for `neg(-2 ** 63)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.neg(123) == -123;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  public func neg(x : Int64) : Int64 { -x };

  /// Returns the sum of `x` and `y`, `x + y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.add(1234, 123) == +1_357;
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
  /// assert Array.foldLeft<Int64, Int64>([1, -2, -3], 0, Int64.add) == -4;
  /// ```
  public func add(x : Int64, y : Int64) : Int64 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.sub(123, 100) == +23;
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
  /// assert Array.foldLeft<Int64, Int64>([1, -2, -3], 0, Int64.sub) == 4;
  /// ```
  public func sub(x : Int64, y : Int64) : Int64 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.mul(123, 10) == +1_230;
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
  /// assert Array.foldLeft<Int64, Int64>([1, -2, -3], 1, Int64.mul) == 6;
  /// ```
  public func mul(x : Int64, y : Int64) : Int64 { x * y };

  /// Returns the signed integer division of `x` by `y`, `x / y`.
  /// Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.div(123, 10) == +12;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Int64, y : Int64) : Int64 { x / y };

  /// Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
  /// which is defined as `x - x / y * y`.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.rem(123, 10) == +3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Int64, y : Int64) : Int64 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`.
  ///
  /// Traps on overflow/underflow and when `y < 0 or y >= 64`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.pow(2, 10) == +1_024;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Int64, y : Int64) : Int64 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitnot(-256 /* 0xffff_ffff_ffff_ff00 */) == +255 // 0xff;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Int64) : Int64 { ^x };

  /// Returns the bitwise "and" of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitand(0xffff, 0x00f0) == +240 // 0xf0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Int64, y : Int64) : Int64 { x & y };

  /// Returns the bitwise "or" of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitor(0xffff, 0x00f0) == +65_535 // 0xffff;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Int64, y : Int64) : Int64 { x | y };

  /// Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitxor(0xffff, 0x00f0) == +65_295 // 0xff0f;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Int64, y : Int64) : Int64 { x ^ y };

  /// Returns the bitwise left shift of `x` by `y`, `x << y`.
  /// The right bits of the shift filled with zeros.
  /// Left-overflowing bits, including the sign bit, are discarded.
  ///
  /// For `y >= 64`, the semantics is the same as for `bitshiftLeft(x, y % 64)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitshiftLeft(1, 8) == +256 // 0x100 equivalent to `2 ** 8`.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Int64, y : Int64) : Int64 { x << y };

  /// Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
  /// The sign bit is retained and the left side is filled with the sign bit.
  /// Right-underflowing bits are discarded, i.e. not rotated to the left side.
  ///
  /// For `y >= 64`, the semantics is the same as for `bitshiftRight(x, y % 64)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitshiftRight(1024, 8) == +4 // equivalent to `1024 / (2 ** 8)`;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Int64, y : Int64) : Int64 { x >> y };

  /// Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
  /// Each left-overflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 64`, the semantics is the same as for `bitrotLeft(x, y % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  ///
  /// assert Int64.bitrotLeft(0x2000_0000_0000_0001, 4) == +18 // 0x12.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Int64, y : Int64) : Int64 { x <<> y };

  /// Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
  /// Each right-underflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 64`, the semantics is the same as for `bitrotRight(x, y % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitrotRight(0x0002_0000_0000_0001, 48) == +65538 // 0x1_0002.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Int64, y : Int64) : Int64 { x <>> y };

  /// Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
  /// If `p >= 64`, the semantics is the same as for `bittest(x, p % 64)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bittest(128, 7);
  /// ```
  public func bittest(x : Int64, p : Nat) : Bool {
    Prim.btstInt64(x, Prim.intToInt64(p))
  };

  /// Returns the value of setting bit `p` in `x` to `1`.
  /// If `p >= 64`, the semantics is the same as for `bitset(x, p % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitset(0, 7) == +128;
  /// ```
  public func bitset(x : Int64, p : Nat) : Int64 {
    x | (1 << Prim.intToInt64(p))
  };

  /// Returns the value of clearing bit `p` in `x` to `0`.
  /// If `p >= 64`, the semantics is the same as for `bitclear(x, p % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitclear(-1, 7) == -129;
  /// ```
  public func bitclear(x : Int64, p : Nat) : Int64 {
    x & ^(1 << Prim.intToInt64(p))
  };

  /// Returns the value of flipping bit `p` in `x`.
  /// If `p >= 64`, the semantics is the same as for `bitclear(x, p % 64)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitflip(255, 7) == +127;
  /// ```
  public func bitflip(x : Int64, p : Nat) : Int64 {
    x ^ (1 << Prim.intToInt64(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitcountNonZero(0xffff) == +16;
  /// ```
  public let bitcountNonZero : (x : Int64) -> Int64 = Prim.popcntInt64;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitcountLeadingZero(0x8000_0000) == +32;
  /// ```
  public let bitcountLeadingZero : (x : Int64) -> Int64 = Prim.clzInt64;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.bitcountTrailingZero(0x0201_0000) == +16;
  /// ```
  public let bitcountTrailingZero : (x : Int64) -> Int64 = Prim.ctzInt64;

  /// Returns the upper (i.e. most significant), lower (least significant)
  /// and in-between bytes of `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.explode 0x33772266aa885511 == (51, 119, 34, 102, 170, 136, 85, 17);
  /// ```
  public let explode : (x : Int64) -> (msb : Nat8, Nat8, Nat8, Nat8, Nat8, Nat8, Nat8, lsb : Nat8) = Prim.explodeInt64;

  /// Returns the sum of `x` and `y`, `x +% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.addWrap(2 ** 62, 2 ** 62) == -9_223_372_036_854_775_808; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Int64, y : Int64) : Int64 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.subWrap(-2 ** 63, 1) == +9_223_372_036_854_775_807; // underflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Int64, y : Int64) : Int64 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.mulWrap(2 ** 32, 2 ** 32) == 0; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Int64, y : Int64) : Int64 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`.
  ///
  /// Wraps on overflow/underflow.
  /// Traps if `y < 0 or y >= 64`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int64.powWrap(2, 63) == -9_223_372_036_854_775_808; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Int64, y : Int64) : Int64 { x **% y };

  /// Returns an iterator over `Int64` values from the first to second argument with an exclusive upper bound.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int64.range(1, 4);
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
  /// let iter = Int64.range(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func range(fromInclusive : Int64, toExclusive : Int64) : Iter.Iter<Int64> {
    if (fromInclusive >= toExclusive) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = fromInclusive;
        public func next() : ?Int64 {
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

  /// Returns an iterator over `Int64` values from the first to second argument, inclusive.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int64.rangeInclusive(1, 3);
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
  /// let iter = Int64.rangeInclusive(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func rangeInclusive(from : Int64, to : Int64) : Iter.Iter<Int64> {
    if (from > to) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = from;
        var done = false;
        public func next() : ?Int64 {
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

  /// Returns an iterator over all Int64 values, from minValue to maxValue.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int64.allValues();
  /// assert iter.next() == ?-9_223_372_036_854_775_808;
  /// assert iter.next() == ?-9_223_372_036_854_775_807;
  /// assert iter.next() == ?-9_223_372_036_854_775_806;
  /// // ...
  /// ```
  public func allValues() : Iter.Iter<Int64> {
    rangeInclusive(minValue, maxValue)
  };

}
