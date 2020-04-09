import Prim "mo:prim";

func testHash(s : Text, h : Word32) {
  Prim.debugPrint("Hash for " # s);
  Prim.debugPrint("Expected: " # debug_show h);
  Prim.debugPrint("Actual:   " # debug_show (Prim.idlHash s));
};

// The lines below can be copied verbatim out of the corresponding JS unit test
// in dev/experimental/js-dfinity-client/tests/unit-tests/idl.js

testHash("", 0);
testHash("id", 23515);
testHash("description", 1595738364);
testHash("short_name", 3261810734);
testHash("Hi ☃", 1419229646);

//SKIP comp
