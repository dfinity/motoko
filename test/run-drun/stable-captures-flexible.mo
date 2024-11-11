import Prim "mo:prim";

actor {
    class TestClass() {
        var flexibleMethod = func() {
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
        var innerFlexible = func() {
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
