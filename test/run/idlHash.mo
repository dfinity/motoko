func testHash(s : Text, h : Word32) {
  Debug.print("Hash for " # s # ":\n");
  Debug.print("Expected: " # Debug.show (word32ToNat(h)) # "\n");
  Debug.print("Actual:   " # Debug.show (word32ToNat(idlHash s)) # "\n");
};

// The lines below can be copied verbatim out of the corresponding JS unit test
// in dev/experimental/js-dfinity-client/tests/unit-tests/idl.js

testHash("", 0);
testHash("id", 23515);
testHash("description", 1595738364);
testHash("short_name", 3261810734);
testHash("Hi ☃", 1419229646);
