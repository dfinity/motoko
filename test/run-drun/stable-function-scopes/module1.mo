import Prim "mo:prim";

module {
  public persistent class TestClass() {
    public func testFunc() {
      Prim.debugPrint("MODULE1 CLASS FUNC");
    };
    public func testFuncExtra() {
      Prim.debugPrint("MODULE1 CLASS FUNC EXTRA");
    };
  };

  public object TestObject {
    public persistent func testFunc() {
      Prim.debugPrint("MODULE1 OBJECT FUNC");
    };
    public persistent func testFuncExtra2() {
      Prim.debugPrint("MODULE1 OBJECT FUNC EXTRA2");
    };
  };

  public persistent func testFunc() {
    Prim.debugPrint("MODULE1 ACTOR FUNC");
  };
};
