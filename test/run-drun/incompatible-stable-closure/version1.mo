import Prim "mo:prim";

persistent actor {
    persistent class TestClass1() {
        var value = 1234;

        public func testFunction() {
            Prim.debugPrint("VERSION 2: " # debug_show(value));
            value += 1;
        }
    };

    let f = TestClass1().testFunction;
    f();
}
