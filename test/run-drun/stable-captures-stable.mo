//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    persistent class TestClass() {
        func otherMethod() {
            Prim.debugPrint("STABLE METHOD");
        };

        var other1 : persistent () -> () = otherMethod;

        public func stableMethod() {
            other1(); // OK, because method declaration is immutable
        };
    };

    let instance = TestClass();
    instance.stableMethod();

    persistent func empty() {
    };

    var function = empty;

    persistent func outer() {
        persistent func otherFunction() {
            Prim.debugPrint("STABLE INNER");
        };

        var other2 : persistent () -> () = otherFunction;

        persistent func inner2() {
            other2(); // OK, because method declaration is immutable
        };

        function := inner2;
    };
    outer();
    function();    
};
