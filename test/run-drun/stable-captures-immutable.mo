import Prim "mo:prim";

actor {
    class TestClass() {
        let flexibleMethod = func() {
            Prim.debugPrint("FLEXIBLE METHOD");
        };

        public func stableMethod() {
            flexibleMethod(); // ERROR
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
            innerFlexible(); // ERROR
        };

        function := inner;
    };
    outer();
    function();    
};
