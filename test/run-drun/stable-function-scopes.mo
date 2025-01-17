//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";
import M1 "stable-function-scopes/module1";
import M2 "stable-function-scopes/module2";

actor {
    class TestClass() {
        public func testFunc() {
            Prim.debugPrint("CLASS FUNC");
        };
        public func testFunc2() {
            Prim.debugPrint("CLASS FUNC2");
        };
    };

    object TestObject {
        public func testFunc() {
            Prim.debugPrint("OBJECT FUNC");
        };
        public func testFunc3() {
            Prim.debugPrint("OBJECT FUNC3");
        };
    };

    func fail() {
        assert(false);
    };

    stable var testInner = fail; // temporary

    func testFunc() {
        Prim.debugPrint("ACTOR FUNC");
        func testFunc() {
            Prim.debugPrint("INNER FUNC");
        };
        testInner := testFunc;
    };

    Prim.debugPrint("---------------------");

    stable let f1 = TestClass().testFunc;
    f1();

    stable let f2 = TestObject.testFunc;
    f2();

    stable let f3 = testFunc;
    f3();

    stable let f4 = M1.TestClass().testFunc;
    f4();

    stable let f5 = M1.TestObject.testFunc;
    f5();

    stable let f6 = M1.testFunc;
    f6();

    stable let f7 = M2.TestClass().testFunc;
    f7();

    stable let f8 = M2.TestObject.testFunc;
    f8();

    stable let f9 = M2.testFunc;
    f9();

    testInner();
};

//CALL upgrade ""
