//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    persistent class TestClass() {
        var flexibleMethod = func() {
            Prim.debugPrint("FLEXIBLE METHOD");
        };

        public func stableMethod() {
            flexibleMethod(); // ERROR
        };
    };

    let instance = TestClass();
    instance.stableMethod();

    persistent func empty() {
    };

    var function = empty;

    persistent func outer() {
        func innerFlexible() {
            Prim.debugPrint("FLEXIBLE INNER");
        };

        persistent func inner() {
            innerFlexible(); // ERROR
        };

        function := inner;
    };
    outer();
    function();    
};
