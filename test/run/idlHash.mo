func testHash(s : Text, h : Word32) {
  debugPrint("Hash for " # s);
  debugPrint("Expected: " # debug_show (word32ToNat(h)));
  debugPrint("Actual:   " # debug_show (word32ToNat(idlHash s)));
};

// The lines below can be copied verbatim out of the corresponding JS unit test
// in dev/experimental/js-dfinity-client/tests/unit-tests/idl.js

testHash("", 0);
testHash("id", 23515);
testHash("description", 1595738364);
testHash("short_name", 3261810734);
testHash("Hi ☃", 1419229646);

//SKIP comp
