import Prim "mo:prim";

persistent actor {
    persistent class TestClass1() {
        var value = "HELLO!";

        public func testFunction() {
            Prim.debugPrint("VERSION 1 " # debug_show(value));
            value #= "!";
        }
    };

    let f = TestClass1().testFunction;
    f();
}
