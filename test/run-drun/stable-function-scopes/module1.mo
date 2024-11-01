import Prim "mo:prim";

module {
  public class TestClass() {
    public func testFunc() {
      Prim.debugPrint("MODULE1 CLASS FUNC");
    };
    public func testFuncExtra() {
      Prim.debugPrint("MODULE1 CLASS FUNC EXTRA");
    };
  };

  public object TestObject {
    public func testFunc() {
      Prim.debugPrint("MODULE1 OBJECT FUNC");
    };
    public func testFuncExtra2() {
      Prim.debugPrint("MODULE1 OBJECT FUNC EXTRA2");
    };
  };

  public func testFunc() {
    Prim.debugPrint("MODULE1 ACTOR FUNC");
  };
};
