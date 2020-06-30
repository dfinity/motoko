import Prim "mo:prim";

let pi = 3.141592653589793238;

Prim.debugPrint "vanilla Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 0));

Prim.debugPrint "fixed Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 5));
Prim.debugPrint(Prim.floatToFormattedText(pi, 10));
Prim.debugPrint(Prim.floatToFormattedText(pi, 17));
Prim.debugPrint(Prim.floatToFormattedText(pi, 18));

Prim.debugPrint "exponential Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 19));
Prim.debugPrint(Prim.floatToFormattedText(pi, 20));
Prim.debugPrint(Prim.floatToFormattedText(pi, 23));
Prim.debugPrint(Prim.floatToFormattedText(pi, 28));
Prim.debugPrint(Prim.floatToFormattedText(pi, 35));
Prim.debugPrint(Prim.floatToFormattedText(pi, 36));

Prim.debugPrint "generic Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 37));
Prim.debugPrint(Prim.floatToFormattedText(pi, 38));
Prim.debugPrint(Prim.floatToFormattedText(pi, 41));
Prim.debugPrint(Prim.floatToFormattedText(pi, 46));
Prim.debugPrint(Prim.floatToFormattedText(pi, 53));
Prim.debugPrint(Prim.floatToFormattedText(pi, 54));

Prim.debugPrint "hex Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 55));
Prim.debugPrint(Prim.floatToFormattedText(pi, 56));
Prim.debugPrint(Prim.floatToFormattedText(pi, 59));
Prim.debugPrint(Prim.floatToFormattedText(pi, 64));
Prim.debugPrint(Prim.floatToFormattedText(pi, 71));
Prim.debugPrint(Prim.floatToFormattedText(pi, 72));

Prim.debugPrint "roundtrip Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 73));
