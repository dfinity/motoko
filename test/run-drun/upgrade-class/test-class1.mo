import Prim "mo:prim";

module {
  public persistent class TestClass() {
    public func print() {
      Prim.debugPrint("Test class version 1");
    };
  };
};
