import Prim "mo:prim";

actor {
    class TestClass() {
        func otherMethod() {
            Prim.debugPrint("STABLE METHOD");
        };

        var other : stable () -> () = otherMethod;

        public func stableMethod() {
            other(); // OK, because method declaration is immutable
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

        var other : stable () -> () = otherFunction;

        func inner() {
            other(); // OK, because method declaration is immutable
        };

        function := inner;
    };
    outer();
    function();    
};
