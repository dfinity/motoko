import Prim "mo:prim";

module Module2 {
  public class TestClass() {
    public func testFunc() {
      Prim.debugPrint("MODULE2 CLASS FUNC");
    };
    public func testFuncExtra() {
      Prim.debugPrint("MODULE2 CLASS FUNC EXTRA");
    };
  };

  public object TestObject {
    public func testFunc() {
      Prim.debugPrint("MODULE2 OBJECT FUNC");
    };
    public func testFuncExtra2() {
      Prim.debugPrint("MODULE2 OBJECT FUNC EXTRA2");
    };
  };

  public func testFunc() {
    Prim.debugPrint("MODULE2 ACTOR FUNC");
  };
};
