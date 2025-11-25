//MOC-FLAG -A M0154,M0194
//MOC-FLAG -W M0154
module A {
  /// @deprecated do not use foo anymore
  public let foo = 5;
};

let x = A.foo;
