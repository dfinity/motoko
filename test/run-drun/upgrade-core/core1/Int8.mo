/// Utility functions on 8-bit signed integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Int8 "mo:core/Int8";
/// ```
import Int "Int";
import Iter "Iter";
import Prim "mo:⛔";
import Order "Order";

module {

  /// 8-bit signed integers.
  public type Int8 = Prim.Types.Int8;

  /// Minimum 8-bit integer value, `-2 ** 7`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.minValue == -128;
  /// ```
  public let minValue : Int8 = -128;

  /// Maximum 8-bit integer value, `+2 ** 7 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.maxValue == +127;
  /// ```
  public let maxValue : Int8 = 127;

  /// Converts an 8-bit signed integer to a signed integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.toInt(123) == (123 : Int);
  /// ```
  public let toInt : Int8 -> Int = Prim.int8ToInt;

  /// Converts a signed integer with infinite precision to an 8-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.fromInt(123) == (+123 : Int8);
  /// ```
  public let fromInt : Int -> Int8 = Prim.intToInt8;

  /// Converts a signed integer with infinite precision to an 8-bit signed integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.fromIntWrap(-123) == (-123 : Int8);
  /// ```
  public let fromIntWrap : Int -> Int8 = Prim.intToInt8Wrap;

  /// Converts a 16-bit signed integer to an 8-bit signed integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.fromInt16(123) == (+123 : Int8);
  /// ```
  public let fromInt16 : Int16 -> Int8 = Prim.int16ToInt8;

  /// Converts an 8-bit signed integer to a 16-bit signed integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.toInt16(123) == (+123 : Int16);
  /// ```
  public let toInt16 : Int8 -> Int16 = Prim.int8ToInt16;

  /// Converts an unsigned 8-bit integer to a signed 8-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.fromNat8(123) == (+123 : Int8);
  /// ```
  public let fromNat8 : Nat8 -> Int8 = Prim.nat8ToInt8;

  /// Converts a signed 8-bit integer to an unsigned 8-bit integer.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.toNat8(-1) == (255 : Nat8); // underflow
  /// ```
  public let toNat8 : Int8 -> Nat8 = Prim.int8ToNat8;

  /// Converts an integer number to its textual representation.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.toText(-123) == "-123";
  /// ```
  public func toText(x : Int8) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`.
  ///
  /// Traps when `x == -2 ** 7` (the minimum `Int8` value).
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.abs(-123) == +123;
  /// ```
  public func abs(x : Int8) : Int8 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.min(+2, -3) == -3;
  /// ```
  public func min(x : Int8, y : Int8) : Int8 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.max(+2, -3) == +2;
  /// ```
  public func max(x : Int8, y : Int8) : Int8 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Int8 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.equal(-1, -1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// let a : Int8 = -123;
  /// let b : Int8 = 123;
  /// assert not Int8.equal(a, b);
  /// ```
  public func equal(x : Int8, y : Int8) : Bool { x == y };

  /// Inequality function for Int8 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.notEqual(-1, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Int8, y : Int8) : Bool { x != y };

  /// "Less than" function for Int8 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.less(-2, 1);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Int8, y : Int8) : Bool { x < y };

  /// "Less than or equal" function for Int8 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.lessOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Int8, y : Int8) : Bool { x <= y };

  /// "Greater than" function for Int8 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.greater(-2, -3);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>`
  /// as a function value at the moment.
  public func greater(x : Int8, y : Int8) : Bool { x > y };

  /// "Greater than or equal" function for Int8 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.greaterOrEqual(-2, -2);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>=`
  /// as a function value at the moment.
  public func greaterOrEqual(x : Int8, y : Int8) : Bool { x >= y };

  /// General-purpose comparison function for `Int8`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.compare(-3, 2) == #less;
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.sort([1, -2, -3] : [Int8], Int8.compare) == [-3, -2, 1];
  /// ```
  public persistent func compare(x : Int8, y : Int8) : Order.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the negation of `x`, `-x`.
  ///
  /// Traps on overflow, i.e. for `neg(-2 ** 7)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.neg(123) == -123;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  public func neg(x : Int8) : Int8 { -x };

  /// Returns the sum of `x` and `y`, `x + y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.add(100, 23) == +123;
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
  /// assert Array.foldLeft<Int8, Int8>([1, -2, -3], 0, Int8.add) == -4;
  /// ```
  public func add(x : Int8, y : Int8) : Int8 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.sub(123, 23) == +100;
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
  /// assert Array.foldLeft<Int8, Int8>([1, -2, -3], 0, Int8.sub) == 4;
  /// ```
  public func sub(x : Int8, y : Int8) : Int8 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.mul(12, 10) == +120;
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
  /// assert Array.foldLeft<Int8, Int8>([1, -2, -3], 1, Int8.mul) == 6;
  /// ```
  public func mul(x : Int8, y : Int8) : Int8 { x * y };

  /// Returns the signed integer division of `x` by `y`, `x / y`.
  /// Rounds the quotient towards zero, which is the same as truncating the decimal places of the quotient.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.div(123, 10) == +12;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Int8, y : Int8) : Int8 { x / y };

  /// Returns the remainder of the signed integer division of `x` by `y`, `x % y`,
  /// which is defined as `x - x / y * y`.
  ///
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.rem(123, 10) == +3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Int8, y : Int8) : Int8 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`.
  ///
  /// Traps on overflow/underflow and when `y < 0 or y >= 8`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.pow(2, 6) == +64;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Int8, y : Int8) : Int8 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitnot(-16 /* 0xf0 */) == +15 // 0x0f;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Int8) : Int8 { ^x };

  /// Returns the bitwise "and" of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitand(0x1f, 0x70) == +16 // 0x10;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Int8, y : Int8) : Int8 { x & y };

  /// Returns the bitwise "or" of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitor(0x0f, 0x70) == +127 // 0x7f;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Int8, y : Int8) : Int8 { x | y };

  /// Returns the bitwise "exclusive or" of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitxor(0x70, 0x7f) == +15 // 0x0f;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Int8, y : Int8) : Int8 { x ^ y };

  /// Returns the bitwise left shift of `x` by `y`, `x << y`.
  /// The right bits of the shift filled with zeros.
  /// Left-overflowing bits, including the sign bit, are discarded.
  ///
  /// For `y >= 8`, the semantics is the same as for `bitshiftLeft(x, y % 8)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftLeft(x, y + y % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitshiftLeft(1, 4) == +16 // 0x10 equivalent to `2 ** 4`.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Int8, y : Int8) : Int8 { x << y };

  /// Returns the signed bitwise right shift of `x` by `y`, `x >> y`.
  /// The sign bit is retained and the left side is filled with the sign bit.
  /// Right-underflowing bits are discarded, i.e. not rotated to the left side.
  ///
  /// For `y >= 8`, the semantics is the same as for `bitshiftRight(x, y % 8)`.
  /// For `y < 0`,  the semantics is the same as for `bitshiftRight (x, y + y % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitshiftRight(64, 4) == +4 // equivalent to `64 / (2 ** 4)`;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Int8, y : Int8) : Int8 { x >> y };

  /// Returns the bitwise left rotatation of `x` by `y`, `x <<> y`.
  /// Each left-overflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 8`, the semantics is the same as for `bitrotLeft(x, y % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitrotLeft(0x11 /* 0b0001_0001 */, 2) == +68 // 0b0100_0100 == 0x44.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Int8, y : Int8) : Int8 { x <<> y };

  /// Returns the bitwise right rotation of `x` by `y`, `x <>> y`.
  /// Each right-underflowing bit is inserted again on the right side.
  /// The sign bit is rotated like other bits, i.e. the rotation interprets the number as unsigned.
  ///
  /// Changes the direction of rotation for negative `y`.
  /// For `y >= 8`, the semantics is the same as for `bitrotRight(x, y % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitrotRight(0x11 /* 0b0001_0001 */, 1) == -120 // 0b1000_1000 == 0x88.;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Int8, y : Int8) : Int8 { x <>> y };

  /// Returns the value of bit `p` in `x`, `x & 2**p == 2**p`.
  /// If `p >= 8`, the semantics is the same as for `bittest(x, p % 8)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bittest(64, 6);
  /// ```
  public func bittest(x : Int8, p : Nat) : Bool {
    Prim.btstInt8(x, Prim.intToInt8(p))
  };

  /// Returns the value of setting bit `p` in `x` to `1`.
  /// If `p >= 8`, the semantics is the same as for `bitset(x, p % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitset(0, 6) == +64;
  /// ```
  public func bitset(x : Int8, p : Nat) : Int8 {
    x | (1 << Prim.intToInt8(p))
  };

  /// Returns the value of clearing bit `p` in `x` to `0`.
  /// If `p >= 8`, the semantics is the same as for `bitclear(x, p % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitclear(-1, 6) == -65;
  /// ```
  public func bitclear(x : Int8, p : Nat) : Int8 {
    x & ^(1 << Prim.intToInt8(p))
  };

  /// Returns the value of flipping bit `p` in `x`.
  /// If `p >= 8`, the semantics is the same as for `bitclear(x, p % 8)`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitflip(127, 6) == +63;
  /// ```
  public func bitflip(x : Int8, p : Nat) : Int8 {
    x ^ (1 << Prim.intToInt8(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitcountNonZero(0x0f) == +4;
  /// ```
  public let bitcountNonZero : (x : Int8) -> Int8 = Prim.popcntInt8;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitcountLeadingZero(0x08) == +4;
  /// ```
  public let bitcountLeadingZero : (x : Int8) -> Int8 = Prim.clzInt8;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.bitcountTrailingZero(0x10) == +4;
  /// ```
  public let bitcountTrailingZero : (x : Int8) -> Int8 = Prim.ctzInt8;

  /// Returns the sum of `x` and `y`, `x +% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.addWrap(2 ** 6, 2 ** 6) == -128; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Int8, y : Int8) : Int8 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.subWrap(-2 ** 7, 1) == +127; // underflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Int8, y : Int8) : Int8 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Wraps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.mulWrap(2 ** 4, 2 ** 4) == 0; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Int8, y : Int8) : Int8 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`.
  ///
  /// Wraps on overflow/underflow.
  /// Traps if `y < 0 or y >= 8`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Int8.powWrap(2, 7) == -128; // overflow
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Int8, y : Int8) : Int8 { x **% y };

  /// Returns an iterator over `Int8` values from the first to second argument with an exclusive upper bound.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int8.range(1, 4);
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
  /// let iter = Int8.range(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func range(fromInclusive : Int8, toExclusive : Int8) : Iter.Iter<Int8> {
    if (fromInclusive >= toExclusive) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = fromInclusive;
        public func next() : ?Int8 {
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

  /// Returns an iterator over `Int8` values from the first to second argument, inclusive.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int8.rangeInclusive(1, 3);
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
  /// let iter = Int8.rangeInclusive(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func rangeInclusive(from : Int8, to : Int8) : Iter.Iter<Int8> {
    if (from > to) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = from;
        var done = false;
        public func next() : ?Int8 {
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

  /// Returns an iterator over all Int8 values, from minValue to maxValue.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Int8.allValues();
  /// assert iter.next() == ?-128;
  /// assert iter.next() == ?-127;
  /// assert iter.next() == ?-126;
  /// // ...
  /// ```
  public func allValues() : Iter.Iter<Int8> {
    rangeInclusive(minValue, maxValue)
  };
}
