/// 16-bit signed integers with checked arithmetic
///
/// Most operations are available as built-in operators (e.g. `1 + 1`).
import Int "Int";
import Prim "mo:⛔";

module {

  /// 16-bit signed integers
  public type Int16 = Prim.Types.Int16;

  /// Conversion.
  public let toInt : Int16 -> Int = Prim.int16ToInt;

  /// Conversion. Traps on overflow/underflow.
  public let fromInt : Int -> Int16  = Prim.intToInt16;

  /// Conversion. Wraps on overflow/underflow.
  public let fromIntWrap : Int -> Int16  = Prim.intToInt16Wrap;

  /// Conversion. Wraps on overflow/underflow.
  public let fromNat16 : Nat16 -> Int16 = Prim.nat16ToInt16;

  /// Conversion. Wraps on overflow/underflow.
  public let toNat16 : Int16 -> Nat16  = Prim.int16ToNat16;

  /// Returns the Text representation of `x`.
  public func toText(x : Int16) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`. Traps when `x = -2^15`.
  public func abs(x : Int16) : Int16 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  public func min(x : Int16, y : Int16) : Int16 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  public func max( x : Int16, y : Int16) : Int16 {
    if (x < y) { y } else { x }
  };

  /// Returns `x == y`.
  public func equal(x : Int16, y : Int16) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Int16, y : Int16) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Int16, y : Int16) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Int16, y : Int16) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Int16, y : Int16) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Int16, y : Int16) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Int16, y : Int16) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the negation of `x`, `-x`. Traps on overflow.
  public func neg(x : Int16) : Int16 { -x; };

  /// Returns the sum of `x` and `y`, `x + y`. Traps on overflow.
  public func add(x : Int16, y : Int16) : Int16 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`. Traps on underflow.
  public func sub(x : Int16, y : Int16) : Int16 { x - y };

  /// Returns the product of `x` and `y`, `x * y`. Traps on overflow.
  public func mul(x : Int16, y : Int16) : Int16 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  public func div(x : Int16, y : Int16) : Int16 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  public func rem(x : Int16, y : Int16) : Int16 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  public func pow(x : Int16, y : Int16) : Int16 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  public func bitnot(x : Int16, y : Int16) : Int16 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  public func bitand(x : Int16, y : Int16) : Int16 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x \| y`.
  public func bitor(x : Int16, y : Int16) : Int16 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  public func bitxor(x : Int16, y : Int16) : Int16 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  public func bitshiftLeft(x : Int16, y : Int16) : Int16 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  public func bitshiftRight(x : Int16, y : Int16) : Int16 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  public func bitrotLeft(x : Int16, y : Int16) : Int16 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  public func bitrotRight(x : Int16, y : Int16) : Int16 { x <>> y };

  /// Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.
  public func bittest(x : Int16, p : Nat) : Bool {
    Prim.btstInt16(x, Prim.intToInt16(p));
  };

  /// Returns the value of setting bit `p mod 16` in `x` to `1`.
  public func bitset(x : Int16, p : Nat) : Int16 {
    x | (1 << Prim.intToInt16(p));
  };

  /// Returns the value of clearing bit `p mod 16` in `x` to `0`.
  public func bitclear(x : Int16, p : Nat) : Int16 {
    x & ^(1 << Prim.intToInt16(p));
  };

  /// Returns the value of flipping bit `p mod 16` in `x`.
  public func bitflip(x : Int16, p : Nat) : Int16 {
    x ^ (1 << Prim.intToInt16(p));
  };

  /// Returns the count of non-zero bits in `x`.
  public let bitcountNonZero : (x : Int16) -> Int16 = Prim.popcntInt16;

  /// Returns the count of leading zero bits in `x`.
  public let bitcountLeadingZero : (x : Int16) -> Int16 = Prim.clzInt16;

  /// Returns the count of trailing zero bits in `x`.
  public let bitcountTrailingZero : (x : Int16) -> Int16 = Prim.ctzInt16;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  public func addWrap(x : Int16, y : Int16) : Int16 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  public func subWrap(x : Int16, y : Int16) : Int16 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  public func mulWrap(x : Int16, y : Int16) : Int16 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
  public func powWrap(x : Int16, y : Int16) : Int16 { x **% y };
}
