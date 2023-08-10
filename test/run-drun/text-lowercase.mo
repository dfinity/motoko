import Prim "mo:⛔";

func check(t : Text, lc : Text) {
    Prim.debugPrint(t);
    Prim.debugPrint(Prim.textLowercase(t));
    assert Prim.textLowercase(t) == lc;
};

check("ABCDCEFGHIJABCDCEFGHIJABCDCEFGHIJABCDCEFGHIJABCDCEFGHIJ 1234 X",
      "abcdcefghijabcdcefghijabcdcefghijabcdcefghijabcdcefghij 1234 x");
