/**
[#mod-Float]
= `Float` -- Floating-point numbers
*/

import Prim "mo:prim";
import Prelude "Prelude";

module {
 
  public let abs : Float -> Float = Prim.floatAbs;  
  public let sqrt : Float -> Float = Prim.floatSqrt;

  public let ceil : Float -> Float = Prim.floatCeil;
  public let floor : Float -> Float = Prim.floatFloor;
  public let trunc : Float -> Float = Prim.floatTrunc;
  public let nearest : Float -> Float = Prim.floatNearest;

  public let min : (Float, Float) -> Float = Prim.floatMin;
  public let max : (Float, Float) -> Float = Prim.floatMax;

  public let sin : Float -> Float = Prim.sin;
  public let cos : Float -> Float = Prim.cos;

  public let toInt64 : Float -> Int64 = Prim.floatToInt64;
  public let ofInt64 : Int64 -> Float = Prim.int64ToFloat;

};
