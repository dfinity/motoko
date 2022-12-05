/// 32-bit signed integers with checked arithmetic
///
/// Most operations are available as built-in operators (e.g. `1 + 1`).
import Int "Int";
import Prim "mo:⛔";

module {

  /// 32-bit signed integers.
  public type Int32 = Prim.Types.Int32;

  /// Conversion.
  public let toInt : Int32 -> Int = Prim.int32ToInt;

  /// Conversion. Traps on overflow/underflow.
  public let fromInt : Int -> Int32  = Prim.intToInt32;

  /// Conversion. Wraps on overflow/underflow.
  public let fromIntWrap : Int -> Int32  = Prim.intToInt32Wrap;

  /// Conversion. Wraps on overflow/underflow.
  public let fromNat32 : Nat32 -> Int32 = Prim.nat32ToInt32;

  /// Conversion. Wraps on overflow/underflow.
  public let toNat32 : Int32 -> Nat32  = Prim.int32ToNat32;

  /// Returns the Text representation of `x`.
  public func toText(x : Int32) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`. Traps when `x = -2^31`.
  public func abs(x : Int32) : Int32 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  public func min(x : Int32, y : Int32) : Int32 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  public func max( x : Int32, y : Int32) : Int32 {
    if (x < y) { y } else { x }
  };

  /// Returns `x == y`.
  public func equal(x : Int32, y : Int32) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Int32, y : Int32) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Int32, y : Int32) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Int32, y : Int32) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Int32, y : Int32) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Int32, y : Int32) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Int32, y : Int32) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the negation of `x`, `-x`. Traps on overflow.
  public func neg(x : Int32) : Int32 { -x; };

  /// Returns the sum of `x` and `y`, `x + y`. Traps on overflow.
  public func add(x : Int32, y : Int32) : Int32 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`. Traps on underflow.
  public func sub(x : Int32, y : Int32) : Int32 { x - y };

  /// Returns the product of `x` and `y`, `x * y`. Traps on overflow.
  public func mul(x : Int32, y : Int32) : Int32 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  public func div(x : Int32, y : Int32) : Int32 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  public func rem(x : Int32, y : Int32) : Int32 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  public func pow(x : Int32, y : Int32) : Int32 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  public func bitnot(x : Int32, y : Int32) : Int32 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  public func bitand(x : Int32, y : Int32) : Int32 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x \| y`.
  public func bitor(x : Int32, y : Int32) : Int32 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  public func bitxor(x : Int32, y : Int32) : Int32 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  public func bitshiftLeft(x : Int32, y : Int32) : Int32 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  public func bitshiftRight(x : Int32, y : Int32) : Int32 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  public func bitrotLeft(x : Int32, y : Int32) : Int32 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  public func bitrotRight(x : Int32, y : Int32) : Int32 { x <>> y };

  /// Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.
  public func bittest(x : Int32, p : Nat) : Bool {
    Prim.btstInt32(x, Prim.intToInt32(p));
  };

  /// Returns the value of setting bit `p mod 16` in `x` to `1`.
  public func bitset(x : Int32, p : Nat) : Int32 {
    x | (1 << Prim.intToInt32(p));
  };

  /// Returns the value of clearing bit `p mod 16` in `x` to `0`.
  public func bitclear(x : Int32, p : Nat) : Int32 {
    x & ^(1 << Prim.intToInt32(p));
  };

  /// Returns the value of flipping bit `p mod 16` in `x`.
  public func bitflip(x : Int32, p : Nat) : Int32 {
    x ^ (1 << Prim.intToInt32(p));
  };

  /// Returns the count of non-zero bits in `x`.
  public let bitcountNonZero : (x : Int32) -> Int32 = Prim.popcntInt32;

  /// Returns the count of leading zero bits in `x`.
  public let bitcountLeadingZero : (x : Int32) -> Int32 = Prim.clzInt32;

  /// Returns the count of trailing zero bits in `x`.
  public let bitcountTrailingZero : (x : Int32) -> Int32 = Prim.ctzInt32;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  public func addWrap(x : Int32, y : Int32) : Int32 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  public func subWrap(x : Int32, y : Int32) : Int32 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  public func mulWrap(x : Int32, y : Int32) : Int32 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
  public func powWrap(x : Int32, y : Int32) : Int32 { x **% y };

}
