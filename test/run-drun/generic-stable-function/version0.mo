import Prim "mo:prim";

actor {
    Prim.debugPrint("Version 0");

    func outer<T>(x : T, op : stable T -> ()) : stable () -> () {
        func inner() {
            op(x);
        };
        return inner;
    };

    var global = false;

    func setBool(x : Bool) {
        Prim.debugPrint("Writing bool");
        global := x;
    };

    stable let stableFunction = outer<Bool>(true, setBool);

    Prim.debugPrint("Before: " # debug_show (global));
    stableFunction();
    Prim.debugPrint("After: " # debug_show (global));
};
