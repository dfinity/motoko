import Prim "mo:prim";

Prim.debugPrint(debug_show(Prim.charToUpper('ö')));
Prim.debugPrint(debug_show(Prim.charToLower('Ö')));
Prim.debugPrint(debug_show(Prim.charIsWhitespace(' ')));

// 12288 (U+3000) = ideographic space
Prim.debugPrint(debug_show(Prim.charIsWhitespace(Prim.word32ToChar(12288))));
