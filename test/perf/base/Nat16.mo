/// 16-bit unsigned integers with checked arithmetic
///
/// Most operations are available as built-in operators (e.g. `1 + 1`).
import Nat "Nat";
import Prim "mo:⛔";

module {

  /// 16-bit natural numbers.
  public type Nat16 = Prim.Types.Nat16;

  /// Conversion.
  public let toNat : Nat16 -> Nat = Prim.nat16ToNat;

  /// Conversion. Traps on overflow/underflow.
  public let fromNat : Nat -> Nat16  = Prim.natToNat16;

  /// Conversion. Wraps on overflow/underflow.
  public let fromIntWrap : Int -> Nat16  = Prim.intToNat16Wrap;

  /// Returns the Text representation of `x`.
  public func toText(x : Nat16) : Text {
    Nat.toText(toNat(x))
  };

  /// Returns the minimum of `x` and `y`.
  public func min(x : Nat16, y : Nat16) : Nat16 {
    if (x < y) { x } else { y }
  };

  /// Returns the maximum of `x` and `y`.
  public func max( x : Nat16, y : Nat16) : Nat16 {
    if (x < y) { y } else { x }
  };

  /// Returns `x == y`.
  public func equal(x : Nat16, y : Nat16) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Nat16, y : Nat16) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Nat16, y : Nat16) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Nat16, y : Nat16) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Nat16, y : Nat16) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Nat16, y : Nat16) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Nat16, y : Nat16) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the sum of `x` and `y`, `x + y`. Traps on overflow.
  public func add(x : Nat16, y : Nat16) : Nat16 { x + y };

  /// Returns the difference of `x` and `y`, `x - y`. Traps on underflow.
  public func sub(x : Nat16, y : Nat16) : Nat16 { x - y };

  /// Returns the product of `x` and `y`, `x * y`. Traps on overflow.
  public func mul(x : Nat16, y : Nat16) : Nat16 { x * y };

  /// Returns the division of `x by y`, `x / y`.
  /// Traps when `y` is zero.
  public func div(x : Nat16, y : Nat16) : Nat16 { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  /// Traps when `y` is zero.
  public func rem(x : Nat16, y : Nat16) : Nat16 { x % y };

  /// Returns `x` to the power of `y`, `x ** y`. Traps on overflow.
  public func pow(x : Nat16, y : Nat16) : Nat16 { x ** y };

  /// Returns the bitwise negation of `x`, `^x`.
  public func bitnot(x : Nat16, y : Nat16) : Nat16 { ^x };

  /// Returns the bitwise and of `x` and `y`, `x & y`.
  public func bitand(x : Nat16, y : Nat16) : Nat16 { x & y };

  /// Returns the bitwise or of `x` and `y`, `x \| y`.
  public func bitor(x : Nat16, y : Nat16) : Nat16 { x | y };

  /// Returns the bitwise exclusive or of `x` and `y`, `x ^ y`.
  public func bitxor(x : Nat16, y : Nat16) : Nat16 { x ^ y };

  /// Returns the bitwise shift left of `x` by `y`, `x << y`.
  public func bitshiftLeft(x : Nat16, y : Nat16) : Nat16 { x << y };

  /// Returns the bitwise shift right of `x` by `y`, `x >> y`.
  public func bitshiftRight(x : Nat16, y : Nat16) : Nat16 { x >> y };

  /// Returns the bitwise rotate left of `x` by `y`, `x <<> y`.
  public func bitrotLeft(x : Nat16, y : Nat16) : Nat16 { x <<> y };

  /// Returns the bitwise rotate right of `x` by `y`, `x <>> y`.
  public func bitrotRight(x : Nat16, y : Nat16) : Nat16 { x <>> y };

  /// Returns the value of bit `p mod 16` in `x`, `(x & 2^(p mod 16)) == 2^(p mod 16)`.
  public func bittest(x : Nat16, p : Nat) : Bool {
    Prim.btstNat16(x, Prim.natToNat16(p));
  };

  /// Returns the value of setting bit `p mod 16` in `x` to `1`.
  public func bitset(x : Nat16, p : Nat) : Nat16 {
    x | (1 << Prim.natToNat16(p));
  };

  /// Returns the value of clearing bit `p mod 16` in `x` to `0`.
  public func bitclear(x : Nat16, p : Nat) : Nat16 {
    x & ^(1 << Prim.natToNat16(p));
  };

  /// Returns the value of flipping bit `p mod 16` in `x`.
  public func bitflip(x : Nat16, p : Nat) : Nat16 {
    x ^ (1 << Prim.natToNat16(p));
  };

  /// Returns the count of non-zero bits in `x`.
  public let bitcountNonZero : (x : Nat16) -> Nat16 = Prim.popcntNat16;

  /// Returns the count of leading zero bits in `x`.
  public let bitcountLeadingZero : (x : Nat16) -> Nat16 = Prim.clzNat16;

  /// Returns the count of trailing zero bits in `x`.
  public let bitcountTrailingZero : (x : Nat16) -> Nat16 = Prim.ctzNat16;

  /// Returns the sum of `x` and `y`, `x +% y`. Wraps on overflow.
  public func addWrap(x : Nat16, y : Nat16) : Nat16 { x +% y };

  /// Returns the difference of `x` and `y`, `x -% y`. Wraps on underflow.
  public func subWrap(x : Nat16, y : Nat16) : Nat16 { x -% y };

  /// Returns the product of `x` and `y`, `x *% y`. Wraps on overflow.
  public func mulWrap(x : Nat16, y : Nat16) : Nat16 { x *% y };

  /// Returns `x` to the power of `y`, `x **% y`. Wraps on overflow.
  public func powWrap(x : Nat16, y : Nat16) : Nat16 { x **% y };

}
