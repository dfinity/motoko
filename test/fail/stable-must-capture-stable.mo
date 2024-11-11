import Prim "mo:prim";

actor {
    stable var version = 0;
    version += 1;

    class TestClass() {
        let flexibleFunction = func() {
            Prim.debugPrint("FLEXIBLE CLASS FUNCTION");
        };

        public func stableFunction() {
            flexibleFunction();
        };
    };

    stable let x = TestClass();
    x.stableFunction();

    func empty() {
    };

    stable var y = empty;

    func outer() {
        var local = 0;

        func inner() {
            Prim.debugPrint("FLEXIBLE INNER FUNCTION " # debug_show(local) # " " # debug_show(version));
            local += 1;
        };

        Prim.debugPrint("SET INNER");
        y := inner;
    };
    if (version == 1) {
        outer();
    };

    y();    
};
