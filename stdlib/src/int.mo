import Prim "mo:prim";
import Prelude "prelude.mo";

module {
  public let abs = Prim.abs;

  public func add(x : Int, y : Int) : Int {
    x + y;
  };

  public func toText(x : Int) : Text {
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

  public let fromInt8  = Prim.int8ToInt;
  public let fromInt16 = Prim.int16ToInt;
  public let fromInt32 = Prim.int32ToInt;
  public let fromInt64 = Prim.int64ToInt;
  public let toInt8  = Prim.intToInt8;
  public let toInt16 = Prim.intToInt16;
  public let toInt32 = Prim.intToInt32;
  public let toInt64 = Prim.intToInt64;
}
