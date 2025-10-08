/// Utility functions on 32-bit signed integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Int32 "mo:core/Int32";
/// ```
import Int "Int";
import Iter "Iter";
import Prim "mo:⛔";
import Order "Order";

module {

  /// 32-bit signed integers.
  public type Int32 = Prim.Types.Int32;

  /// Minimum 32-bit integer value, `-2 ** 31`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.minValue == -2_147_483_648;
  /// ```
  public let minValue : Int32 = -2_147_483_648;

  /// Maximum 32-bit integer value, `+2 ** 31 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.maxValue == +2_147_483_647;
  /// ```
  public let maxValue : Int32 = 2_147_483_647;

  /// Converts a 32-bit signed integer to a signed integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.toInt(123_456) == (123_456 : Int);
  /// ```
  public let toInt : Int32 -> Int = Prim.int32ToInt;

  /// Converts a signed integer with infinite precision to a 32-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.fromInt(123_456) == (+123_456 : Int32);
  /// ```
  public let fromInt : Int -> Int32 = Prim.intToInt32;

  /// Converts a signed integer with infinite precision to a 32-bit signed integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.fromIntWrap(-123_456) == (-123_456 : Int);
  /// ```
  public let fromIntWrap : Int -> Int32 = Prim.intToInt32Wrap;

  /// Converts a 16-bit signed integer to a 32-bit signed integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.fromInt16(-123) == (-123 : Int32);
  /// ```
  public let fromInt16 : Int16 -> Int32 = Prim.int16ToInt32;

  /// Converts a 32-bit signed integer to a 16-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.toInt16(-123) == (-123 : Int16);
  /// ```
  public let toInt16 : Int32 -> Int16 = Prim.int32ToInt16;

  /// Converts a 64-bit signed integer to a 32-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.fromInt64(-123_456) == (-123_456 : Int32);
  /// ```
  public let fromInt64 : Int64 -> Int32 = Prim.int64ToInt32;

  /// Converts a 32-bit signed integer to a 64-bit signed integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.toInt64(-123_456) == (-123_456 : Int64);
  /// ```
  public let toInt64 : Int32 -> Int64 = Prim.int32ToInt64;

  /// Converts an unsigned 32-bit integer to a signed 32-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.fromNat32(123_456) == (+123_456 : Int32);
  /// ```
  public let fromNat32 : Nat32 -> Int32 = Prim.nat32ToInt32;

  /// Converts a signed 32-bit integer to an unsigned 32-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.toNat32(-1) == (4_294_967_295 : Nat32); // underflow
  /// ```
  public let toNat32 : Int32 -> Nat32 = Prim.int32ToNat32;

  /// Returns the Text representation of `x`. Textual representation _do not_
  /// contain underscores to represent commas.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.toText(-123456) == "-123456";
  /// ```
  public func toText(x : Int32) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`.
  ///
  /// Traps when `x == -2 ** 31` (the minimum `Int32` value).
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.abs(-123456) == +123_456;
  /// ```
  public func abs(x : Int32) : Int32 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.min(+2, -3) == -3;
  /// ```
  public func min(x : Int32, y : Int32) : Int32 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.max(+2, -3) == +2;
  /// ```
  public func max(x : Int32, y : Int32) : Int32 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Int32 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.equal(-1, -1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// let a : Int32 = -123;
  /// let b : Int32 = 123;
  /// assert not Int32.equal(a, b);
  /// ```
  public func equal(x : Int32, y : Int32) : Bool { x == y };

  /// Inequality function for Int32 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.notEqual(-1, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Int32, y : Int32) : Bool { x != y };

  /// "Less than" function for Int32 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.less(-2, 1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Int32, y : Int32) : Bool { x < y };

  /// "Less than or equal" function for Int32 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.lessOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Int32, y : Int32) : Bool { x <= y };

  /// "Greater than" function for Int32 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.greater(-2, -3);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>`
  /// as a function value at the moment.
  public func greater(x : Int32, y : Int32) : Bool { x > y };

  /// "Greater than or equal" function for Int32 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.greaterOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>=`
  /// as a function value at the moment.
  public func greaterOrEqual(x : Int32, y : Int32) : Bool { x >= y };

  /// General-purpose comparison function for `Int32`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.compare(-3, 2) == #less;
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.sort([1, -2, -3] : [Int32], Int32.compare) == [-3, -2, 1];
  /// ```
  public persistent func compare(x : Int32, y : Int32) : Order.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the negation of `x`, `-x`.
  ///
  /// Traps on overflow, i.e. for `neg(-2 ** 31)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.neg(123) == -123;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  public func neg(x : Int32) : Int32 { -x };

  /// Returns the sum of `x` and `y`, `x + y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.add(100, 23) == +123;
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
  /// assert Array.foldLeft<Int32, Int32>([1, -2, -3], 0, Int32.add) == -4;
  /// ```
  public func add(x : Int32, y : Int32) : Int32 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.sub(1234, 123) == +1_111;
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
  /// assert Array.foldLeft<Int32, Int32>([1, -2, -3], 0, Int32.sub) == 4;
  /// ```
  public func sub(x : Int32, y : Int32) : Int32 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.mul(123, 100) == +12_300;
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
  /// assert Array.foldLeft<Int32, Int32>([1, -2, -3], 1, Int32.mul) == 6;
  /// ```
  public func mul(x : Int32, y : Int32) : Int32 { x * y };

  /// Returns the signed integer division of `x` by `y`, `x / y`.
  /// Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.div(123, 10) == +12;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Int32, y : Int32) : Int32 { x / y };

  /// Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
  /// which is defined as `x - x / y * y`.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.rem(123, 10) == +3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Int32, y : Int32) : Int32 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`.
  ///
  /// Traps on overflow/underflow and when `y < 0 or y >= 32`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.pow(2, 10) == +1_024;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Int32, y : Int32) : Int32 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitnot(-256 /* 0xffff_ff00 */) == +255 // 0xff;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Int32) : Int32 { ^x };

  /// Returns the bitwise "and" of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitand(0xffff, 0x00f0) == +240 // 0xf0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Int32, y : Int32) : Int32 { x & y };

  /// Returns the bitwise "or" of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitor(0xffff, 0x00f0) == +65_535 // 0xffff;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Int32, y : Int32) : Int32 { x | y };

  /// Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitxor(0xffff, 0x00f0) == +65_295 // 0xff0f;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Int32, y : Int32) : Int32 { x ^ y };

  /// Returns the bitwise left shift of `x` by `y`, `x << y`.
  /// The right bits of the shift filled with zeros.
  /// Left-overflowing bits, including the sign bit, are discarded.
  ///
  /// For `y >= 32`, the semantics is the same as for `bitshiftLeft(x, y % 32)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitshiftLeft(1, 8) == +256 // 0x100 equivalent to `2 ** 8`.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Int32, y : Int32) : Int32 { x << y };

  /// Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
  /// The sign bit is retained and the left side is filled with the sign bit.
  /// Right-underflowing bits are discarded, i.e. not rotated to the left side.
  ///
  /// For `y >= 32`, the semantics is the same as for `bitshiftRight(x, y % 32)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitshiftRight(1024, 8) == +4 // equivalent to `1024 / (2 ** 8)`;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Int32, y : Int32) : Int32 { x >> y };

  /// Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
  /// Each left-overflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 32`, the semantics is the same as for `bitrotLeft(x, y % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitrotLeft(0x2000_0001, 4) == +18 // 0x12.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Int32, y : Int32) : Int32 { x <<> y };

  /// Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
  /// Each right-underflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 32`, the semantics is the same as for `bitrotRight(x, y % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitrotRight(0x0002_0001, 8) == +16_777_728 // 0x0100_0200.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Int32, y : Int32) : Int32 { x <>> y };

  /// Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
  /// If `p >= 32`, the semantics is the same as for `bittest(x, p % 32)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bittest(128, 7);
  /// ```
  public func bittest(x : Int32, p : Nat) : Bool {
    Prim.btstInt32(x, Prim.intToInt32(p))
  };

  /// Returns the value of setting bit `p` in `x` to `1`.
  /// If `p >= 32`, the semantics is the same as for `bitset(x, p % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitset(0, 7) == +128;
  /// ```
  public func bitset(x : Int32, p : Nat) : Int32 {
    x | (1 << Prim.intToInt32(p))
  };

  /// Returns the value of clearing bit `p` in `x` to `0`.
  /// If `p >= 32`, the semantics is the same as for `bitclear(x, p % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitclear(-1, 7) == -129;
  /// ```
  public func bitclear(x : Int32, p : Nat) : Int32 {
    x & ^(1 << Prim.intToInt32(p))
  };

  /// Returns the value of flipping bit `p` in `x`.
  /// If `p >= 32`, the semantics is the same as for `bitclear(x, p % 32)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitflip(255, 7) == +127;
  /// ```
  public func bitflip(x : Int32, p : Nat) : Int32 {
    x ^ (1 << Prim.intToInt32(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitcountNonZero(0xffff) == +16;
  /// ```
  public let bitcountNonZero : (x : Int32) -> Int32 = Prim.popcntInt32;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitcountLeadingZero(0x8000) == +16;
  /// ```
  public let bitcountLeadingZero : (x : Int32) -> Int32 = Prim.clzInt32;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.bitcountTrailingZero(0x0201_0000) == +16;
  /// ```
  public let bitcountTrailingZero : (x : Int32) -> Int32 = Prim.ctzInt32;

  /// Returns the upper (i.e. most significant), lower (least significant)
  /// and in-between bytes of `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.explode 0x66885511 == (102, 136, 85, 17);
  /// ```
  public let explode : (x : Int32) -> (msb : Nat8, Nat8, Nat8, lsb : Nat8) = Prim.explodeInt32;

  /// Returns the sum of `x` and `y`, `x +% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.addWrap(2 ** 30, 2 ** 30) == -2_147_483_648; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Int32, y : Int32) : Int32 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.subWrap(-2 ** 31, 1) == +2_147_483_647; // underflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Int32, y : Int32) : Int32 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.mulWrap(2 ** 16, 2 ** 16) == 0; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Int32, y : Int32) : Int32 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`.
  ///
  /// Wraps on overflow/underflow.
  /// Traps if `y < 0 or y >= 32`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int32.powWrap(2, 31) == -2_147_483_648; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Int32, y : Int32) : Int32 { x **% y };

  /// Returns an iterator over `Int32` values from the first to second argument with an exclusive upper bound.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int32.range(1, 4);
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
  /// let iter = Int32.range(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func range(fromInclusive : Int32, toExclusive : Int32) : Iter.Iter<Int32> {
    if (fromInclusive >= toExclusive) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = fromInclusive;
        public func next() : ?Int32 {
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

  /// Returns an iterator over `Int32` values from the first to second argument, inclusive.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int32.rangeInclusive(1, 3);
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
  /// let iter = Int32.rangeInclusive(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func rangeInclusive(from : Int32, to : Int32) : Iter.Iter<Int32> {
    if (from > to) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = from;
        var done = false;
        public func next() : ?Int32 {
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

  /// Returns an iterator over all Int32 values, from minValue to maxValue.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int32.allValues();
  /// assert iter.next() == ?-2_147_483_648;
  /// assert iter.next() == ?-2_147_483_647;
  /// assert iter.next() == ?-2_147_483_646;
  /// // ...
  /// ```
  public func allValues() : Iter.Iter<Int32> {
    rangeInclusive(minValue, maxValue)
  };

}
