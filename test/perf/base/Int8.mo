/// 8-bit signed integers with checked arithmetic
///
/// Most operations are available as built-in operators (e.g. `1 + 1`).
import Int "Int";
import Prim "mo:⛔";

module {

  /// 8-bit signed integers.
  public type Int8 = Prim.Types.Int8;

  /// Conversion.
  public let toInt : Int8 -> Int = Prim.int8ToInt;

  /// Conversion. Traps on overflow/underflow.
  public let fromInt : Int -> Int8  = Prim.intToInt8;

  /// Conversion. Wraps on overflow/underflow.
  public let fromIntWrap : Int -> Int8  = Prim.intToInt8Wrap;

  /// Conversion. Wraps on overflow/underflow.
  public let fromNat8 : Nat8 -> Int8 = Prim.nat8ToInt8;

  /// Conversion. Wraps on overflow/underflow.
  public let toNat8 : Int8 -> Nat8  = Prim.int8ToNat8;

  /// Returns the Text representation of `x`.
  public func toText(x : Int8) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`. Traps when `x = -2^7`.
  public func abs(x : Int8) : Int8 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  public func min(x : Int8, y : Int8) : Int8 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  public func max( x : Int8, y : Int8) : Int8 {
    if (x < y) { y } else { x }
  };

  /// Returns `x == y`.
  public func equal(x : Int8, y : Int8) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Int8, y : Int8) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Int8, y : Int8) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Int8, y : Int8) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Int8, y : Int8) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Int8, y : Int8) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Int8, y : Int8) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the negation of `x`, `-x`. Traps on overflow.
  public func neg(x : Int8) : Int8 { -x; };

  /// Returns the sum of `x` and `y`, `x + y`. Traps on overflow.
  public func add(x : Int8, y : Int8) : Int8 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`. Traps on underflow.
  public func sub(x : Int8, y : Int8) : Int8 { x - y };

  /// Returns the product of `x` and `y`, `x * y`. Traps on overflow.
  public func mul(x : Int8, y : Int8) : Int8 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  public func div(x : Int8, y : Int8) : Int8 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  public func rem(x : Int8, y : Int8) : Int8 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  public func pow(x : Int8, y : Int8) : Int8 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  public func bitnot(x : Int8, y : Int8) : Int8 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  public func bitand(x : Int8, y : Int8) : Int8 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x \| y`.
  public func bitor(x : Int8, y : Int8) : Int8 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  public func bitxor(x : Int8, y : Int8) : Int8 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  public func bitshiftLeft(x : Int8, y : Int8) : Int8 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  public func bitshiftRight(x : Int8, y : Int8) : Int8 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  public func bitrotLeft(x : Int8, y : Int8) : Int8 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  public func bitrotRight(x : Int8, y : Int8) : Int8 { x <>> y };

  /// Returns the value of bit `p mod 8` in `x`, `(x & 2^(p mod 8)) == 2^(p mod 8)`.
  public func bittest(x : Int8, p : Nat) : Bool {
    Prim.btstInt8(x, Prim.intToInt8(p));
  };

  /// Returns the value of setting bit `p mod 8` in `x` to `1`.
  public func bitset(x : Int8, p : Nat) : Int8 {
    x | (1 << Prim.intToInt8(p));
  };

  /// Returns the value of clearing bit `p mod 8` in `x` to `0`.
  public func bitclear(x : Int8, p : Nat) : Int8 {
    x & ^(1 << Prim.intToInt8(p));
  };

  /// Returns the value of flipping bit `p mod 8` in `x`.
  public func bitflip(x : Int8, p : Nat) : Int8 {
    x ^ (1 << Prim.intToInt8(p));
  };

  /// Returns the count of non-zero bits in `x`.
  public let bitcountNonZero : (x : Int8) -> Int8 = Prim.popcntInt8;

  /// Returns the count of leading zero bits in `x`.
  public let bitcountLeadingZero : (x : Int8) -> Int8 = Prim.clzInt8;

  /// Returns the count of trailing zero bits in `x`.
  public let bitcountTrailingZero : (x : Int8) -> Int8 = Prim.ctzInt8;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  public func addWrap(x : Int8, y : Int8) : Int8 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  public func subWrap(x : Int8, y : Int8) : Int8 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  public func mulWrap(x : Int8, y : Int8) : Int8 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
  public func powWrap(x : Int8, y : Int8) : Int8 { x **% y };

}
