import { debugPrint; shiftLeft } = "mo:⛔"

class range(x : Nat32, y : Nat32) {
    var i = x;
    public func next() : ?Nat32 { if (i > y) null else {let j = i; i += 1; ?j} };
};

debugPrint (debug_show shiftLeft(42, 7));
debugPrint (debug_show shiftLeft(42, 24));
debugPrint (debug_show shiftLeft(42, 25));
debugPrint (debug_show shiftLeft(42, 26));
debugPrint (debug_show shiftLeft(42, 25 + 32)); // 57
debugPrint (debug_show shiftLeft(42, 25 + 64)); // 89
debugPrint (debug_show shiftLeft(42, 125));
debugPrint (debug_show shiftLeft(0, 125));
debugPrint (debug_show shiftLeft(10 ** 10, 25));
for (i in range(0, 200)) { debugPrint (debug_show (i, shiftLeft(42, i))) }
