//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";
import M1 "stable-function-scopes/module1";
import M2 "stable-function-scopes/module2";

persistent actor {
    persistent class TestClass() {
        public func testFunc() {
            Prim.debugPrint("CLASS FUNC");
        };
        public func testFunc2() {
            Prim.debugPrint("CLASS FUNC2");
        };
    };

    object TestObject {
        public persistent func testFunc() {
            Prim.debugPrint("OBJECT FUNC");
        };
        public persistent func testFunc3() {
            Prim.debugPrint("OBJECT FUNC3");
        };
    };

    persistent func fail() {
        assert(false);
    };

    var testInner = fail; // temporary

    persistent func testFunc() {
        Prim.debugPrint("ACTOR FUNC");
        persistent func testFunc() {
            Prim.debugPrint("INNER FUNC");
        };
        testInner := testFunc;
    };

    Prim.debugPrint("---------------------");

    let f1 = TestClass().testFunc;
    f1();

    let f2 = TestObject.testFunc;
    f2();

    let f3 = testFunc;
    f3();

    let f4 = M1.TestClass().testFunc;
    f4();

    let f5 = M1.TestObject.testFunc;
    f5();

    let f6 = M1.testFunc;
    f6();

    let f7 = M2.TestClass().testFunc;
    f7();

    let f8 = M2.TestObject.testFunc;
    f8();

    let f9 = M2.testFunc;
    f9();

    testInner();
};

//CALL upgrade ""
