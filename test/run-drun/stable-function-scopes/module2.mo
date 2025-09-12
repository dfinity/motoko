import Prim "mo:prim";

module Module2 {
  public persistent class TestClass() {
    public func testFunc() {
      Prim.debugPrint("MODULE2 CLASS FUNC");
    };
    public func testFuncExtra() {
      Prim.debugPrint("MODULE2 CLASS FUNC EXTRA");
    };
  };

  public object TestObject {
    public persistent func testFunc() {
      Prim.debugPrint("MODULE2 OBJECT FUNC");
    };
    public persistent func testFuncExtra2() {
      Prim.debugPrint("MODULE2 OBJECT FUNC EXTRA2");
    };
  };

  public persistent func testFunc() {
    Prim.debugPrint("MODULE2 ACTOR FUNC");
  };
};
