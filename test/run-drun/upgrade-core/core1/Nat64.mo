/// Utility functions on 64-bit unsigned integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Nat64 "mo:core/Nat64";
/// ```
import Nat "Nat";
import Iter "Iter";
import Prim "mo:⛔";
import Order "Order";

module {

  /// 64-bit natural numbers.
  public type Nat64 = Prim.Types.Nat64;

  /// Maximum 64-bit natural number. `2 ** 64 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.maxValue == (18446744073709551615 : Nat64);
  /// ```
  public let maxValue : Nat64 = 18446744073709551615;

  /// Converts a 64-bit unsigned integer to an unsigned integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.toNat(123) == (123 : Nat);
  /// ```
  public let toNat : Nat64 -> Nat = Prim.nat64ToNat;

  /// Converts an unsigned integer with infinite precision to a 64-bit unsigned integer.
  ///
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.fromNat(123) == (123 : Nat64);
  /// ```
  public let fromNat : Nat -> Nat64 = Prim.natToNat64;

  /// Converts a 32-bit unsigned integer to a 64-bit unsigned integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.fromNat32(123) == (123 : Nat64);
  /// ```
  public func fromNat32(x : Nat32) : Nat64 {
    Prim.nat32ToNat64(x)
  };

  /// Converts a 64-bit unsigned integer to a 32-bit unsigned integer.
  ///
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.toNat32(123) == (123 : Nat32);
  /// ```
  public func toNat32(x : Nat64) : Nat32 {
    Prim.nat64ToNat32(x)
  };

  /// Converts a signed integer with infinite precision to a 64-bit unsigned integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.fromIntWrap(123) == (123 : Nat64);
  /// ```
  public let fromIntWrap : Int -> Nat64 = Prim.intToNat64Wrap;

  /// Converts `x` to its textual representation. Textual representation _do not_
  /// contain underscores to represent commas.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.toText(1234) == ("1234" : Text);
  /// ```
  public func toText(x : Nat64) : Text {
    Nat.toText(toNat(x))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.min(123, 456) == (123 : Nat64);
  /// ```
  public func min(x : Nat64, y : Nat64) : Nat64 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.max(123, 456) == (456 : Nat64);
  /// ```
  public func max(x : Nat64, y : Nat64) : Nat64 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Nat64 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.equal(1, 1);
  /// assert (1 : Nat64) == (1 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// let a : Nat64 = 111;
  /// let b : Nat64 = 222;
  /// assert not Nat64.equal(a, b);
  /// ```
  public func equal(x : Nat64, y : Nat64) : Bool { x == y };

  /// Inequality function for Nat64 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.notEqual(1, 2);
  /// assert (1 : Nat64) != (2 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Nat64, y : Nat64) : Bool { x != y };

  /// "Less than" function for Nat64 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.less(1, 2);
  /// assert (1 : Nat64) < (2 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Nat64, y : Nat64) : Bool { x < y };

  /// "Less than or equal" function for Nat64 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.lessOrEqual(1, 2);
  /// assert (1 : Nat64) <= (2 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Nat64, y : Nat64) : Bool { x <= y };

  /// "Greater than" function for Nat64 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.greater(2, 1);
  /// assert (2 : Nat64) > (1 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>`
  /// as a function value at the moment.
  public func greater(x : Nat64, y : Nat64) : Bool { x > y };

  /// "Greater than or equal" function for Nat64 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.greaterOrEqual(2, 1);
  /// assert (2 : Nat64) >= (1 : Nat64);
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>=`
  /// as a function value at the moment.
  public func greaterOrEqual(x : Nat64, y : Nat64) : Bool { x >= y };

  /// General purpose comparison function for `Nat64`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.compare(2, 3) == #less;
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:core/Array";
  /// assert Array.sort([2, 3, 1] : [Nat64], Nat64.compare) == [1, 2, 3];
  /// ```
  public persistent func compare(x : Nat64, y : Nat64) : Order.Order {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the sum of `x` and `y`, `x + y`.
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.add(1, 2) == 3;
  /// assert (1 : Nat64) + (2 : Nat64) == 3;
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
  /// assert Array.foldLeft<Nat64, Nat64>([2, 3, 1], 0, Nat64.add) == 6;
  /// ```
  public func add(x : Nat64, y : Nat64) : Nat64 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  /// Traps on underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.sub(3, 1) == 2;
  /// assert (3 : Nat64) - (1 : Nat64) == 2;
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
  /// assert Array.foldLeft<Nat64, Nat64>([2, 3, 1], 10, Nat64.sub) == 4;
  /// ```
  public func sub(x : Nat64, y : Nat64) : Nat64 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.mul(2, 3) == 6;
  /// assert (2 : Nat64) * (3 : Nat64) == 6;
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
  /// assert Array.foldLeft<Nat64, Nat64>([2, 3, 1], 1, Nat64.mul) == 6;
  /// ```
  public func mul(x : Nat64, y : Nat64) : Nat64 { x * y };

  /// Returns the quotient of `x` divided by `y`, `x / y`.
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.div(6, 2) == 3;
  /// assert (6 : Nat64) / (2 : Nat64) == 3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Nat64, y : Nat64) : Nat64 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.rem(6, 4) == 2;
  /// assert (6 : Nat64) % (4 : Nat64) == 2;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Nat64, y : Nat64) : Nat64 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.pow(2, 3) == 8;
  /// assert (2 : Nat64) ** (3 : Nat64) == 8;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Nat64, y : Nat64) : Nat64 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitnot(0) == 18446744073709551615;
  /// assert ^(0 : Nat64) == 18446744073709551615;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Nat64) : Nat64 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitand(1, 3) == 1;
  /// assert (1 : Nat64) & (3 : Nat64) == 1;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Nat64, y : Nat64) : Nat64 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitor(1, 3) == 3;
  /// assert (1 : Nat64) | (3 : Nat64) == 3;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Nat64, y : Nat64) : Nat64 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitxor(1, 3) == 2;
  /// assert (1 : Nat64) ^ (3 : Nat64) == 2;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Nat64, y : Nat64) : Nat64 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitshiftLeft(1, 3) == 8;
  /// assert (1 : Nat64) << (3 : Nat64) == 8;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Nat64, y : Nat64) : Nat64 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitshiftRight(8, 3) == 1;
  /// assert (8 : Nat64) >> (3 : Nat64) == 1;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Nat64, y : Nat64) : Nat64 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitrotLeft(1, 3) == 8;
  /// assert (1 : Nat64) <<> (3 : Nat64) == 8;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Nat64, y : Nat64) : Nat64 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitrotRight(8, 3) == 1;
  /// assert (8 : Nat64) <>> (3 : Nat64) == 1;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Nat64, y : Nat64) : Nat64 { x <>> y };

  /// Returns the value of bit `p mod 64` in `x`, `(x & 2^(p mod 64)) == 2^(p mod 64)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bittest(5, 2);
  /// ```
  public func bittest(x : Nat64, p : Nat) : Bool {
    Prim.btstNat64(x, Prim.natToNat64(p))
  };

  /// Returns the value of setting bit `p mod 64` in `x` to `1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitset(5, 1) == 7;
  /// ```
  public func bitset(x : Nat64, p : Nat) : Nat64 {
    x | (1 << Prim.natToNat64(p))
  };

  /// Returns the value of clearing bit `p mod 64` in `x` to `0`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitclear(5, 2) == 1;
  /// ```
  public func bitclear(x : Nat64, p : Nat) : Nat64 {
    x & ^(1 << Prim.natToNat64(p))
  };

  /// Returns the value of flipping bit `p mod 64` in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitflip(5, 2) == 1;
  /// ```
  public func bitflip(x : Nat64, p : Nat) : Nat64 {
    x ^ (1 << Prim.natToNat64(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitcountNonZero(5) == 2;
  /// ```
  public let bitcountNonZero : (x : Nat64) -> Nat64 = Prim.popcntNat64;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitcountLeadingZero(5) == 61;
  /// ```
  public let bitcountLeadingZero : (x : Nat64) -> Nat64 = Prim.clzNat64;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.bitcountTrailingZero(16) == 4;
  /// ```
  public let bitcountTrailingZero : (x : Nat64) -> Nat64 = Prim.ctzNat64;

  /// Returns the upper (i.e. most significant), lower (least significant)
  /// and in-between bytes of `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.explode 0xbb772266aa885511 == (187, 119, 34, 102, 170, 136, 85, 17);
  /// ```
  public let explode : (x : Nat64) -> (msb : Nat8, Nat8, Nat8, Nat8, Nat8, Nat8, Nat8, lsb : Nat8) = Prim.explodeNat64;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.addWrap(Nat64.maxValue, 1) == 0;
  /// assert Nat64.maxValue +% (1 : Nat64) == 0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Nat64, y : Nat64) : Nat64 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.subWrap(0, 1) == 18446744073709551615;
  /// assert (0 : Nat64) -% (1 : Nat64) == 18446744073709551615;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Nat64, y : Nat64) : Nat64 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.mulWrap(4294967296, 4294967296) == 0;
  /// assert (4294967296 : Nat64) *% (4294967296 : Nat64) == 0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Nat64, y : Nat64) : Nat64 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert Nat64.powWrap(2, 64) == 0;
  /// assert (2 : Nat64) **% (64 : Nat64) == 0;
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Nat64, y : Nat64) : Nat64 { x **% y };

  /// Returns an iterator over `Nat64` values from the first to second argument with an exclusive upper bound.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Nat64.range(1, 4);
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
  /// let iter = Nat64.range(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func range(fromInclusive : Nat64, toExclusive : Nat64) : Iter.Iter<Nat64> {
    if (fromInclusive >= toExclusive) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = fromInclusive;
        public func next() : ?Nat64 {
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

  /// Returns an iterator over `Nat64` values from the first to second argument, inclusive.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Nat64.rangeInclusive(1, 3);
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
  /// let iter = Nat64.rangeInclusive(4, 1);
  /// assert iter.next() == null; // empty iterator
  /// ```
  public func rangeInclusive(from : Nat64, to : Nat64) : Iter.Iter<Nat64> {
    if (from > to) {
      Iter.empty()
    } else {
      Iter.Iter(object {
        var n = from;
        var done = false;
        public func next() : ?Nat64 {
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

  /// Returns an iterator over all Nat64 values, from 0 to maxValue.
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let iter = Nat64.allValues();
  /// assert iter.next() == ?0;
  /// assert iter.next() == ?1;
  /// assert iter.next() == ?2;
  /// // ...
  /// ```
  public func allValues() : Iter.Iter<Nat64> {
    rangeInclusive(0, maxValue)
  };

}
