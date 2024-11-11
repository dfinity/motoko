import Prim "mo:prim";

actor {
    class TestClass() {
        let flexibleMethod = func() {
            Prim.debugPrint("FLEXIBLE METHOD");
        };

        public func stableMethod() {
            flexibleMethod(); // OK, because method declaration is immutable
        };
    };

    stable let instance = TestClass();
    instance.stableMethod();

    func empty() {
    };

    stable var function = empty;

    func outer() {
        let innerFlexible = func() {
            Prim.debugPrint("FLEXIBLE INNER");
        };

        func inner() {
            innerFlexible(); // OK, because function declaration is immutable
        };

        function := inner;
    };
    outer();
    function();    
};
