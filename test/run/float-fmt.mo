import Prim "mo:⛔";

let pi = 3.141592653589793238;

Prim.debugPrint "fixed Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 0, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 1, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 4, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 9, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 16, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 17, 0));
Prim.debugPrint(Prim.floatToFormattedText(pi, 20, 0));

Prim.debugPrint "exponential Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 0, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 1, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 4, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 9, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 16, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 17, 1));
Prim.debugPrint(Prim.floatToFormattedText(pi, 20, 1));

Prim.debugPrint "generic Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 0, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 1, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 4, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 9, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 16, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 17, 2));
Prim.debugPrint(Prim.floatToFormattedText(pi, 20, 2));

Prim.debugPrint "hex Float";
Prim.debugPrint(Prim.floatToFormattedText(pi, 0, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 1, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 4, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 9, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 16, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 17, 3));
Prim.debugPrint(Prim.floatToFormattedText(pi, 20, 3));
