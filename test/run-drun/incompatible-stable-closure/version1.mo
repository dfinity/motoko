import Prim "mo:prim";

actor {
    class TestClass1() {
        var value = 1234;

        public func testFunction() {
            Prim.debugPrint("VERSION 2: " # debug_show(value));
            value += 1;
        }
    };

    stable let f = TestClass1().testFunction;
    f();
}
