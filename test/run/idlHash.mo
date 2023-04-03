import Prim "mo:⛔";

func testHash(s : Text, h : Nat32) {
  Prim.debugPrint("Hash for " # s);
  Prim.debugPrint("Expected: " # debug_show h);
  Prim.debugPrint("Actual:   " # debug_show (Prim.idlHash s));
};

testHash("", 0);
testHash("id", 23515);
testHash("description", 1595738364);
testHash("short_name", 3261810734);
testHash("Hi ☃", 1419229646);

//SKIP comp
