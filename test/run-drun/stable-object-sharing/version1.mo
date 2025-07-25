import Prim "mo:prim";

actor {
    func invalid1() {
        Prim.trap("must not be called");
    };

    func invalid2() {
        Prim.trap("must not be called");
    };

    func invalid3() {
        Prim.trap("must not be called");
    };

    func stableFunction1() {
        Prim.debugPrint("Stable function 1 upgraded");
    };

    func stableFunction2() {
        Prim.debugPrint("Stable function 2 upgraded");
    };

    type FullObject = {
        function1 : stable () -> ();
        function2 : stable () -> ();
        function3 : stable () -> ();
    };

    func invalid() : FullObject {
        {
            function1 = invalid1;
            function2 = invalid2;
            function3 = invalid3;
        };
    };

    var _f1 = stableFunction1;
    var _f2 = stableFunction2;

    stable let partialView1 : Any = invalid();

    stable let partialView2 : {
        function1 : stable () -> ();
    } = invalid();

    stable let partialView3 : {
        function2 : stable () -> ();
    } = invalid();

    partialView2.function1();
    partialView3.function2();
};
