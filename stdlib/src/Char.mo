/**
[#mod-Char]
= `Char` -- Characters
*/

import Prim "mo:prim";
module {
  public let isDigit : Char -> Bool = func(char) {
    Prim.charToWord32(char) - Prim.charToWord32('0') <= (9 : Word32)
  };

}
