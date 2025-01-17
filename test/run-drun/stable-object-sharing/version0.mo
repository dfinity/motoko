import Prim "mo:prim";

actor {
    func stableFunction1() {
        Prim.debugPrint("Stable function 1");
    };

    func stableFunction2() {
        Prim.debugPrint("Stable function 2");
    };

    func stableFunction3() {
        Prim.debugPrint("Stable function 3");
    };

    flexible let fullObject = {
        function1 = stableFunction1;
        function2 = stableFunction2;
        function3 = stableFunction3;
    };

    stable let partialView1 : Any = fullObject;

    stable let partialView2 : {
        function1 : stable () -> ();
    } = fullObject;

    stable let partialView3 : {
        function2 : stable () -> ();
    } = fullObject;

    partialView2.function1();
    partialView3.function2();
};
