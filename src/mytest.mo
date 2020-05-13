/**
[#mod-Int]
= `Int` -- Integer numbers

Most operations on natural numbers (e.g. addition) are available as built-in operators (`1 + 1`).
This module provides conversion functions.

The conversions `toInt*` will trap if the number is out of bounds.
*/

import Prim "mo:prim";
import Prelude "Prelude";

module {
  /**
  Returns the absolute value of the number
  */
  public let abs : Int -> Nat = Prim.abs;

  // Remove?
  public func add(x : Int, y : Int) : Int {
    x + y;
  };

  public let toText : Int -> Text = func(x) {
    if (x == 0) {
      return "0";
    };

    let isNegative = x < 0;
    var int = if isNegative (-x) else x;

    var text = "";
    let base = 10;

    while (int > 0) {
      let rem = int % base;
      text := (switch (rem) {
        case 0 "0";
        case 1 "1";
        case 2 "2";
        case 3 "3";
        case 4 "4";
        case 5 "5";
        case 6 "6";
        case 7 "7";
        case 8 "8";
        case 9 "9";
        case _ Prelude.unreachable();
      }) # text;
      int := int / base;
    };

    return if isNegative ("-" # text) else text;
  };

  public let fromInt8  : Int8  -> Int = Prim.int8ToInt;
  public let fromInt16 : Int16 -> Int = Prim.int16ToInt;
  public let fromInt32 : Int32 -> Int = Prim.int32ToInt;
  public let fromInt64 : Int64 -> Int = Prim.int64ToInt;

  public let toInt8    : Int -> Int8  = Prim.intToInt8;
  public let toInt16   : Int -> Int16 = Prim.intToInt16;
  public let toInt32   : Int -> Int32 = Prim.intToInt32;
  public let toInt64   : Int -> Int64 = Prim.intToInt64;

  public let min : (Int, Int) -> Int = func(x,y) {
    if (x < y) x else y;
  };

  public let max : (Int, Int) -> Int = func(x,y) {
    if (x < y) y else x;
  };
}
