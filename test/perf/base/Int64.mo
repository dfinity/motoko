/// 64-bit signed integers with checked arithmetic
///
/// Most operations are available as built-in operators (e.g. `1 + 1`).
import Int "Int";
import Prim "mo:⛔";

module {

  /// 64-bit signed integers.
  public type Int64 = Prim.Types.Int64;

  /// Conversion.
  public let toInt : Int64 -> Int = Prim.int64ToInt;

  /// Conversion. Traps on overflow/underflow.
  public let fromInt : Int -> Int64  = Prim.intToInt64;

  /// Conversion. Wraps on overflow/underflow.
  public let fromIntWrap : Int -> Int64  = Prim.intToInt64Wrap;

  /// Conversion. Wraps on overflow/underflow.
  public let fromNat64 : Nat64 -> Int64 = Prim.nat64ToInt64;

  /// Conversion. Wraps on overflow/underflow.
  public let toNat64 : Int64 -> Nat64  = Prim.int64ToNat64;

  /// Returns the Text representation of `x`.
  public func toText(x : Int64) : Text {
    Int.toText(toInt(x))
  };

  /// Returns the absolute value of `x`. Traps when `x = -2^63`.
  public func abs(x : Int64) : Int64 {
    fromInt(Int.abs(toInt(x)))
  };

  /// Returns the minimum of `x` and `y`.
  public func min(x : Int64, y : Int64) : Int64 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  public func max( x : Int64, y : Int64) : Int64 {
    if (x < y) { y } else { x }
  };

  /// Returns `x == y`.
  public func equal(x : Int64, y : Int64) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Int64, y : Int64) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Int64, y : Int64) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Int64, y : Int64) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Int64, y : Int64) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Int64, y : Int64) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Int64, y : Int64) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the negation of `x`, `-x`. Traps on overflow.
  public func neg(x : Int64) : Int64 { -x; };

  /// Returns the sum of `x` and `y`, `x + y`. Traps on overflow.
  public func add(x : Int64, y : Int64) : Int64 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`. Traps on underflow.
  public func sub(x : Int64, y : Int64) : Int64 { x - y };

  /// Returns the product of `x` and `y`, `x * y`. Traps on overflow.
  public func mul(x : Int64, y : Int64) : Int64 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  public func div(x : Int64, y : Int64) : Int64 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  public func rem(x : Int64, y : Int64) : Int64 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  public func pow(x : Int64, y : Int64) : Int64 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  public func bitnot(x : Int64, y : Int64) : Int64 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  public func bitand(x : Int64, y : Int64) : Int64 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x \| y`.
  public func bitor(x : Int64, y : Int64) : Int64 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  public func bitxor(x : Int64, y : Int64) : Int64 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  public func bitshiftLeft(x : Int64, y : Int64) : Int64 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  public func bitshiftRight(x : Int64, y : Int64) : Int64 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  public func bitrotLeft(x : Int64, y : Int64) : Int64 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  public func bitrotRight(x : Int64, y : Int64) : Int64 { x <>> y };

  /// Returns the value of bit `p mod 64` in `x`, `(x & 2^(p mod 64)) == 2^(p mod 64)`.
  public func bittest(x : Int64, p : Nat) : Bool {
    Prim.btstInt64(x, Prim.intToInt64(p));
  };

  /// Returns the value of setting bit `p mod 64` in `x` to `1`.
  public func bitset(x : Int64, p : Nat) : Int64 {
    x | (1 << Prim.intToInt64(p));
  };

  /// Returns the value of clearing bit `p mod 64` in `x` to `0`.
  public func bitclear(x : Int64, p : Nat) : Int64 {
    x & ^(1 << Prim.intToInt64(p));
  };

  /// Returns the value of flipping bit `p mod 64` in `x`.
  public func bitflip(x : Int64, p : Nat) : Int64 {
    x ^ (1 << Prim.intToInt64(p));
  };

  /// Returns the count of non-zero bits in `x`.
  public let bitcountNonZero : (x : Int64) -> Int64 = Prim.popcntInt64;

  /// Returns the count of leading zero bits in `x`.
  public let bitcountLeadingZero : (x : Int64) -> Int64 = Prim.clzInt64;

  /// Returns the count of trailing zero bits in `x`.
  public let bitcountTrailingZero : (x : Int64) -> Int64 = Prim.ctzInt64;


  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  public func addWrap(x : Int64, y : Int64) : Int64 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  public func subWrap(x : Int64, y : Int64) : Int64 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  public func mulWrap(x : Int64, y : Int64) : Int64 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow. Traps if `y < 0`.
  public func powWrap(x : Int64, y : Int64) : Int64 { x **% y };

}
