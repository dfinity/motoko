import Prim "mo:prim";

actor {
    persistent func stableFunction1() {
        Prim.debugPrint("Stable function 1");
    };

    persistent func stableFunction2() {
        Prim.debugPrint("Stable function 2");
    };

    persistent func stableFunction3() {
        Prim.debugPrint("Stable function 3");
    };

    flexible let fullObject = {
        function1 = stableFunction1;
        function2 = stableFunction2;
        function3 = stableFunction3;
    };

    stable let partialView1 : Any = fullObject;

    stable let partialView2 : {
        function1 : persistent () -> ();
    } = fullObject;

    stable let partialView3 : {
        function2 : persistent () -> ();
    } = fullObject;

    partialView2.function1();
    partialView3.function2();
};
