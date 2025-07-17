import Prim "mo:prim";

actor {
    class TestClass1() {
        var value = "HELLO!";

        public func testFunction() {
            Prim.debugPrint("VERSION 1 " # debug_show(value));
            value #= "!";
        }
    };

    stable let f = TestClass1().testFunction;
    f();
}
