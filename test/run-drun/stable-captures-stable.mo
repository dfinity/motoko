//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

actor {
    class TestClass() {
        func otherMethod() {
            Prim.debugPrint("STABLE METHOD");
        };

        var other1 : stable () -> () = otherMethod;

        public func stableMethod() {
            other1(); // OK, because method declaration is immutable
        };
    };

    stable let instance = TestClass();
    instance.stableMethod();

    func empty() {
    };

    stable var function = empty;

    func outer() {
        func otherFunction() {
            Prim.debugPrint("STABLE INNER");
        };

        var other2 : stable () -> () = otherFunction;

        func inner2() {
            other2(); // OK, because method declaration is immutable
        };

        function := inner2;
    };
    outer();
    function();    
};
