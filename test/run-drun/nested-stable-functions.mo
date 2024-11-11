import Prim "mo:prim";

actor {
    stable var version = 0;
    version += 1;

    func empty() {
    };

    stable var y = empty;

    func outer() {
        var local = 0;

        func inner() {
            Prim.debugPrint("STABLE INNER FUNCTION " # debug_show(local) # " " # debug_show(version));
            local += 1;
        };

        Prim.debugPrint("STABLE OUTER FUNCTION" # debug_show(local));
        y := inner;
    };
    if (version == 1) {
        outer();
    };

    y();
    y();
};
