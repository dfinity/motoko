/// 64-bit Floating-point numbers

import Prim "mo:⛔";
import Int "Int";

module {

  /// 64-bit floating point numbers.
  public type Float = Prim.Types.Float;

  /// Ratio of the circumference of a circle to its diameter.
  public let pi : Float = 3.14159265358979323846; // taken from musl math.h

  /// Base of the natural logarithm.
  public let e : Float  = 2.7182818284590452354;  // taken from musl math.h

  /// Returns the absolute value of `x`.
  public let abs : (x : Float) -> Float = Prim.floatAbs;

  /// Returns the square root of `x`.
  public let sqrt : (x : Float) -> Float = Prim.floatSqrt;

  /// Returns the smallest integral float greater than or equal to `x`.
  public let ceil : (x : Float) -> Float = Prim.floatCeil;

  /// Returns the largest integral float less than or equal to `x`.
  public let floor : (x : Float) -> Float = Prim.floatFloor;

  /// Returns the nearest integral float not greater in magnitude than `x`.
  public let trunc : (x : Float) -> Float = Prim.floatTrunc;

  /// Returns the nearest integral float to `x`.
  public let nearest : (x : Float) -> Float = Prim.floatNearest;

  /// Returns `x` if `x` and `y` have same sign, otherwise `x` with negated sign.
  public let copySign : (x : Float, y : Float) -> Float = Prim.floatCopySign;

  /// Returns the smaller value of `x` and `y`.
  public let min : (x : Float, y :  Float) -> Float = Prim.floatMin;

  /// Returns the larger value of `x` and `y`.
  public let max : (x : Float, y : Float) -> Float = Prim.floatMax;

  /// Returns the sine of the radian angle `x`.
  public let sin : (x : Float) -> Float = Prim.sin;

  /// Returns the cosine of the radian angle `x`.
  public let cos : (x : Float) -> Float = Prim.cos;

  /// Returns the tangent of the radian angle `x`.
  public let tan : (x : Float) -> Float = Prim.tan;

  /// Returns the arc sine of `x` in radians.
  public let arcsin: (x : Float) -> Float = Prim.arcsin;

  /// Returns the arc cosine of `x` in radians.
  public let arccos : (x : Float) -> Float = Prim.arccos;

  /// Returns the arc tangent of `x` in radians.
  public let arctan : (x : Float) -> Float = Prim.arctan;

  /// Given `(y,x)`, returns the arc tangent in radians of `y/x` based on the signs of both values to determine the correct quadrant.
  public let arctan2 : (y : Float, x : Float) -> Float = Prim.arctan2;

  /// Returns the value of `e` raised to the `x`-th power.
  public let exp : (x : Float) -> Float = Prim.exp;

  /// Returns the natural logarithm (base-`e`) of `x`.
  public let log : (x : Float) -> Float = Prim.log;

  /// Formatting. `format(fmt, x)` formats `x` to `Text` according to the
  /// formatting directive `fmt`, which can take one of the following forms:
  ///
  /// * `#fix prec` as fixed-point format with `prec` digits
  /// * `#exp prec` as exponential format with `prec` digits
  /// * `#gen prec` as generic format with `prec` digits
  /// * `#hex prec` as hexadecimal format with `prec` digits
  /// * `#exact` as exact format that can be decoded without loss.
  public func format
    (fmt : { #fix : Nat8; #exp : Nat8; #gen : Nat8; #hex : Nat8; #exact }, x : Float) : Text =
    switch fmt {
      case (#fix(prec)) { Prim.floatToFormattedText(x, prec, 0) };
      case (#exp(prec)) { Prim.floatToFormattedText(x, prec, 1) };
      case (#gen(prec)) { Prim.floatToFormattedText(x, prec, 2) };
      case (#hex(prec)) { Prim.floatToFormattedText(x, prec, 3) };
      case (#exact) { Prim.floatToFormattedText(x, 17, 2) };
    };

  /// Conversion to Text. Use `format(fmt, x)` for more detailed control.
  public let toText : Float -> Text = Prim.floatToText;

  /// Conversion to Int64 by truncating Float, equivalent to `toInt64(trunc(f))`
  public let toInt64 : Float -> Int64 = Prim.floatToInt64;

  /// Conversion from Int64.
  public let fromInt64 : Int64 -> Float = Prim.int64ToFloat;

  /// Conversion to Int.
  public let toInt : Float -> Int = Prim.floatToInt;

  /// Conversion from Int. May result in `Inf`.
  public let fromInt : Int -> Float = Prim.intToFloat;

  /// Returns `x == y`.
  public func equal(x : Float, y : Float) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Float, y : Float) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Float, y : Float) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Float, y : Float) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Float, y : Float) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Float, y : Float) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Float, y : Float) : { #less; #equal; #greater} {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Returns the negation of `x`, `-x` .
  public func neq(x : Float) : Float { -x; };

  /// Returns the sum of `x` and `y`, `x + y`.
  public func add(x : Float, y : Float) : Float { x + y };

  /// Returns the difference of `x` and `y`, `x - y`.
  public func sub(x : Float, y : Float) : Float { x - y };

  /// Returns the product of `x` and `y`, `x * y`.
  public func mul(x : Float, y : Float) : Float { x * y };

  /// Returns the division of `x` by `y`, `x / y`.
  public func div(x : Float, y : Float) : Float { x / y };

  /// Returns the remainder of `x` divided by `y`, `x % y`.
  public func rem(x : Float, y : Float) : Float { x % y };

  /// Returns `x` to the power of `y`, `x ** y`.
  public func pow(x : Float, y : Float) : Float { x ** y };

};
