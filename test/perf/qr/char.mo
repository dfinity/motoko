/**
[#mod-Char]
= `Char` -- Characters
*/

import Prim "mo:⛔";
module {
  public let isDigit : Char -> Bool = func(char) {
    Prim.charToNat32(char) -% Prim.charToNat32('0') <= (9 : Nat32)
  };

}
