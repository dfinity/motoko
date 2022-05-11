import { debugPrint; shiftLeft } = "mo:⛔"

debugPrint (debug_show shiftLeft(42, 7));
debugPrint (debug_show shiftLeft(42, 24));
debugPrint (debug_show shiftLeft(42, 25));
debugPrint (debug_show shiftLeft(42, 26));
debugPrint (debug_show shiftLeft(42, 25 + 32)); // 57
debugPrint (debug_show shiftLeft(42, 25 + 64)); // 89
debugPrint (debug_show shiftLeft(42, 125));
debugPrint (debug_show shiftLeft(0, 125));
debugPrint (debug_show shiftLeft(10 ** 10, 25))
