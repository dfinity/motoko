func testHash(s : Text, h : Word32) {
  print("Hash for " # s # ":\n");
  print("Expected: " # debug_show (word32ToNat(h)) # "\n");
  print("Actual:   " # debug_show (word32ToNat(idlHash s)) # "\n");
};

testHash("", 0);
testHash("id", 23515);
testHash("description", 1595738364);
testHash("short_name", 3261810734);
testHash("Hi ☃", 1419229646);

