/// Provides utility functions on 32-bit unsigned integers.
///
/// Note that most operations are available as built-in operators (e.g. `1 + 1`).
///
/// Import from the base library to use this module.
/// ```motoko name=import
/// import Nat32 "mo:base/Nat32";
/// ```
import Nat "Nat";
import Prim "mo:⛔";

module {

  /// 32-bit natural numbers.
  public type Nat32 = Prim.Types.Nat32;

  /// Maximum 32-bit natural number. `2 ** 32 - 1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.maximumValue; // => 4294967295 : Nat32
  /// ```
  public let maximumValue = 4294967295 : Nat32;

  /// Converts a 32-bit unsigned integer to an unsigned integer with infinite precision.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.toNat(123); // => 123 : Nat
  /// ```
  public let toNat : Nat32 -> Nat = Prim.nat32ToNat;

  /// Converts an unsigned integer with infinite precision to a 32-bit unsigned integer.
  ///
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.fromNat(123); // => 123 : Nat32
  /// ```
  public let fromNat : Nat -> Nat32 = Prim.natToNat32;

  /// Converts a 16-bit unsigned integer to a 32-bit unsigned integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.fromNat16(123); // => 123 : Nat32
  /// ```
  public func fromNat16(x : Nat16) : Nat32 {
    Prim.nat16ToNat32(x)
  };

  /// Converts a 32-bit unsigned integer to a 16-bit unsigned integer.
  ///
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.toNat16(123); // => 123 : Nat16
  /// ```
  public func toNat16(x : Nat32) : Nat16 {
    Prim.nat32ToNat16(x)
  };

  /// Converts a 64-bit unsigned integer to a 32-bit unsigned integer.
  ///
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.fromNat64(123); // => 123 : Nat32
  /// ```
  public func fromNat64(x : Nat64) : Nat32 {
    Prim.nat64ToNat32(x)
  };

  /// Converts a 32-bit unsigned integer to a 64-bit unsigned integer.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.toNat64(123); // => 123 : Nat64
  /// ```
  public func toNat64(x : Nat32) : Nat64 {
    Prim.nat32ToNat64(x)
  };

  /// Converts a signed integer with infinite precision to a 32-bit unsigned integer.
  ///
  /// Traps on overflow/underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.fromIntWrap(123); // => 123 : Nat32
  /// ```
  public let fromIntWrap : Int -> Nat32 = Prim.intToNat32Wrap;

  /// Converts `x` to its textual representation. Textual representation _do not_
  /// contain underscores to represent commas.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.toText(1234); // => "1234" : Text
  /// ```
  public func toText(x : Nat32) : Text {
    Nat.toText(toNat(x))
  };

  /// Returns the minimum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.min(123, 456); // => 123 : Nat32
  /// ```
  public func min(x : Nat32, y : Nat32) : Nat32 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.max(123, 456); // => 456 : Nat32
  /// ```
  public func max(x : Nat32, y : Nat32) : Nat32 {
    if (x < y) { y } else { x }
  };

  /// Equality function for Nat32 types.
  /// This is equivalent to `x == y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.equal(1, 1); // => true
  /// (1 : Nat32) == (1 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `==` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `==`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Buffer "mo:base/Buffer";
  ///
  /// let buffer1 = Buffer.Buffer<Nat32>(3);
  /// let buffer2 = Buffer.Buffer<Nat32>(3);
  /// Buffer.equal(buffer1, buffer2, Nat32.equal) // => true
  /// ```
  public func equal(x : Nat32, y : Nat32) : Bool { x == y };

  /// Inequality function for Nat32 types.
  /// This is equivalent to `x != y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.notEqual(1, 2); // => true
  /// (1 : Nat32) != (2 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `!=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `!=`
  /// as a function value at the moment.
  public func notEqual(x : Nat32, y : Nat32) : Bool { x != y };

  /// "Less than" function for Nat32 types.
  /// This is equivalent to `x < y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.less(1, 2); // => true
  /// (1 : Nat32) < (2 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<`
  /// as a function value at the moment.
  public func less(x : Nat32, y : Nat32) : Bool { x < y };

  /// "Less than or equal" function for Nat32 types.
  /// This is equivalent to `x <= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.lessOrEqual(1, 2); // => true
  /// (1 : Nat32) <= (2 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<=`
  /// as a function value at the moment.
  public func lessOrEqual(x : Nat32, y : Nat32) : Bool { x <= y };

  /// "Greater than" function for Nat32 types.
  /// This is equivalent to `x > y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.greater(2, 1); // => true
  /// (2 : Nat32) > (1 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>`
  /// as a function value at the moment.
  public func greater(x : Nat32, y : Nat32) : Bool { x > y };

  /// "Greater than or equal" function for Nat32 types.
  /// This is equivalent to `x >= y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.greaterOrEqual(2, 1); // => true
  /// (2 : Nat32) >= (1 : Nat32) // => true
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>=` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>=`
  /// as a function value at the moment.
  public func greaterOrEqual(x : Nat32, y : Nat32) : Bool { x >= y };

  /// General purpose comparison function for `Nat32`. Returns the `Order` (
  /// either `#less`, `#equal`, or `#greater`) of comparing `x` with `y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.compare(2, 3) // => #less
  /// ```
  ///
  /// This function can be used as value for a high order function, such as a sort function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:base/Array";
  /// Array.sort([2, 3, 1] : [Nat32], Nat32.compare) // => [1, 2, 3]
  /// ```
  public func compare(x : Nat32, y : Nat32) : { #less; #equal; #greater } {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  /// Returns the sum of `x` and `y`, `x + y`.
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.add(1, 2); // => 3
  /// (1 : Nat32) + (2 : Nat32) // => 3
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:base/Array";
  /// Array.foldLeft<Nat32, Nat32>([2, 3, 1], 0, Nat32.add) // => 6
  /// ```
  public func add(x : Nat32, y : Nat32) : Nat32 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  /// Traps on underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.sub(2, 1); // => 1
  /// (2 : Nat32) - (1 : Nat32) // => 1
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:base/Array";
  /// Array.foldLeft<Nat32, Nat32>([2, 3, 1], 20, Nat32.sub) // => 14
  /// ```
  public func sub(x : Nat32, y : Nat32) : Nat32 { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  /// Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.mul(2, 3); // => 6
  /// (2 : Nat32) * (3 : Nat32) // => 6
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*`
  /// as a function value at the moment.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Array "mo:base/Array";
  /// Array.foldLeft<Nat32, Nat32>([2, 3, 1], 1, Nat32.mul) // => 6
  /// ```
  public func mul(x : Nat32, y : Nat32) : Nat32 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.div(6, 2); // => 3
  /// (6 : Nat32) / (2 : Nat32) // => 3
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `/` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `/`
  /// as a function value at the moment.
  public func div(x : Nat32, y : Nat32) : Nat32 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.rem(6, 4); // => 2
  /// (6 : Nat32) % (4 : Nat32) // => 2
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `%`
  /// as a function value at the moment.
  public func rem(x : Nat32, y : Nat32) : Nat32 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.pow(2, 3); // => 8
  /// (2 : Nat32) ** (3 : Nat32) // => 8
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**`
  /// as a function value at the moment.
  public func pow(x : Nat32, y : Nat32) : Nat32 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitnot(0) // => 4294967295
  /// ^(0 : Nat32) // => 4294967295
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitnot(x : Nat32) : Nat32 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitand(1, 3); // => 1
  /// (1 : Nat32) & (3 : Nat32) // => 1
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `&` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `&`
  /// as a function value at the moment.
  public func bitand(x : Nat32, y : Nat32) : Nat32 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x | y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitor(1, 3); // => 3
  /// (1 : Nat32) | (3 : Nat32) // => 3
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `|` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `|`
  /// as a function value at the moment.
  public func bitor(x : Nat32, y : Nat32) : Nat32 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitxor(1, 3); // => 2
  /// (1 : Nat32) ^ (3 : Nat32) // => 2
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `^` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `^`
  /// as a function value at the moment.
  public func bitxor(x : Nat32, y : Nat32) : Nat32 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitshiftLeft(1, 3); // => 8
  /// (1 : Nat32) << (3 : Nat32) // => 8
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<`
  /// as a function value at the moment.
  public func bitshiftLeft(x : Nat32, y : Nat32) : Nat32 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitshiftRight(8, 3); // => 1
  /// (8 : Nat32) >> (3 : Nat32) // => 1
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `>>`
  /// as a function value at the moment.
  public func bitshiftRight(x : Nat32, y : Nat32) : Nat32 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitrotLeft(1, 3); // => 8
  /// (1 : Nat32) <<> (3 : Nat32) // => 8
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<<>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<<>`
  /// as a function value at the moment.
  public func bitrotLeft(x : Nat32, y : Nat32) : Nat32 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.bitrotRight(1, 1); // => 2147483648
  /// (1 : Nat32) <>> (1 : Nat32) // => 2147483648
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `<>>` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `<>>`
  /// as a function value at the moment.
  public func bitrotRight(x : Nat32, y : Nat32) : Nat32 { x <>> y };

  /// Returns the value of bit `p mod 32` in `x`, `(x & 2^(p mod 32)) == 2^(p mod 32)`.
  /// This is equivalent to checking if the `p`-th bit is set in `x`, using 0 indexing.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bittest(5, 2); // => true
  /// ```
  public func bittest(x : Nat32, p : Nat) : Bool {
    Prim.btstNat32(x, Prim.natToNat32(p))
  };

  /// Returns the value of setting bit `p mod 32` in `x` to `1`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitset(5, 1); // => 7
  /// ```
  public func bitset(x : Nat32, p : Nat) : Nat32 {
    x | (1 << Prim.natToNat32(p))
  };

  /// Returns the value of clearing bit `p mod 32` in `x` to `0`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitclear(5, 2); // => 1
  /// ```
  public func bitclear(x : Nat32, p : Nat) : Nat32 {
    x & ^(1 << Prim.natToNat32(p))
  };

  /// Returns the value of flipping bit `p mod 32` in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitflip(5, 2); // => 1
  /// ```
  public func bitflip(x : Nat32, p : Nat) : Nat32 {
    x ^ (1 << Prim.natToNat32(p))
  };

  /// Returns the count of non-zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitcountNonZero(5); // => 2
  /// ```
  public let bitcountNonZero : (x : Nat32) -> Nat32 = Prim.popcntNat32;

  /// Returns the count of leading zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitcountLeadingZero(5); // => 29
  /// ```
  public let bitcountLeadingZero : (x : Nat32) -> Nat32 = Prim.clzNat32;

  /// Returns the count of trailing zero bits in `x`.
  ///
  /// Example:
  /// ```motoko include=import
  /// Nat32.bitcountTrailingZero(16); // => 4
  /// ```
  public let bitcountTrailingZero : (x : Nat32) -> Nat32 = Prim.ctzNat32;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.addWrap(4294967295, 1); // => 0
  /// (4294967295 : Nat32) +% (1 : Nat32) // => 0
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `+%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `+%`
  /// as a function value at the moment.
  public func addWrap(x : Nat32, y : Nat32) : Nat32 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.subWrap(0, 1); // => 4294967295
  /// (0 : Nat32) -% (1 : Nat32) // => 4294967295
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `-%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `-%`
  /// as a function value at the moment.
  public func subWrap(x : Nat32, y : Nat32) : Nat32 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.mulWrap(2147483648, 2); // => 0
  /// (2147483648 : Nat32) *% (2 : Nat32) // => 0
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `*%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `*%`
  /// as a function value at the moment.
  public func mulWrap(x : Nat32, y : Nat32) : Nat32 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
  ///
  /// Example:
  /// ```motoko include=import
  /// ignore Nat32.powWrap(2, 32); // => 0
  /// (2 : Nat32) **% (32 : Nat32) // => 0
  /// ```
  ///
  /// Note: The reason why this function is defined in this library (in addition
  /// to the existing `**%` operator) is so that you can use it as a function
  /// value to pass to a higher order function. It is not possible to use `**%`
  /// as a function value at the moment.
  public func powWrap(x : Nat32, y : Nat32) : Nat32 { x **% y };

}
