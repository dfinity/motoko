import Prim "mo:prim";

actor {
    Prim.debugPrint("Version 0");

    func outer<T>(x : T, op : stable T -> ()) : stable () -> () {
        func inner() {
            op(x);
        };
        return inner;
    };

    var global = "";

    stable func setBool(x : Bool) {
        Prim.debugPrint("Calling setBool");
    };
    func setText(x : Text) {
        Prim.debugPrint("Writing text");
        global := x;
    };

    stable let stableFunction = outer<Text>("Hello", setText);

    Prim.debugPrint("Before: " # debug_show (global));
    stableFunction(); // stays with old `X = Bool` and calls the old `setBool`.
    Prim.debugPrint("After: " # debug_show (global));
};
