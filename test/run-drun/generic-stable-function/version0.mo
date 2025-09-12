import Prim "mo:prim";

persistent actor {
    Prim.debugPrint("Version 0");

    persistent func outer<T>(x : T, op : persistent T -> ()) : persistent () -> () {
        persistent func inner() {
            op(x);
        };
        return inner;
    };

    transient var global = false;

    persistent func setBool(x : Bool) {
        Prim.debugPrint("Writing bool");
        global := x;
    };

    let stableFunction = outer<Bool>(true, setBool);

    Prim.debugPrint("Before: " # debug_show (global));
    stableFunction();
    Prim.debugPrint("After: " # debug_show (global));
};
