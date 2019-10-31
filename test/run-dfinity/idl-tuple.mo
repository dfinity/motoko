let a = actor {
  public func len2() : async (Int,Int) {
    (23, 42)
  }
};

a.len2();

//  CALL len2 0x4449444c016a027c7c00000100 [console:logAny]
