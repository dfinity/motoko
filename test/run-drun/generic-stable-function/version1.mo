import Prim "mo:prim";

persistent actor {
    Prim.debugPrint("Version 1");

    persistent func outer<T>(x : T, op : persistent T -> ()) : persistent () -> () {
        persistent func inner() {
            op(x);
        };
        return inner;
    };

    transient var global = "";

    persistent func setBool(x : Bool) {
        Prim.debugPrint("Calling setBool");
    };

    // create function reference to retain
    let _ignore = setBool;

    persistent func setText(x : Text) {
        Prim.debugPrint("Writing text");
        global := x;
    };

    let stableFunction = outer<Text>("Hello", setText);

    Prim.debugPrint("Before: " # debug_show (global));
    stableFunction(); // stays with old `X = Bool` and calls the old `setBool`.
    Prim.debugPrint("After: " # debug_show (global));
};
