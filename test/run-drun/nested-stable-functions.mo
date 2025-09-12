//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    var version = 0;
    version += 1;

    persistent func empty() {
    };

    var y = empty;

    persistent func outer() {
        var local = 0;

        persistent func inner() {
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
