import Prim "mo:prim";

persistent actor {
    persistent func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 1");
    };

    persistent func optionalFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION VERSION 1");
    };

    var retained = optionalFunction;
    retained := requiredFunction;
    retained();
}
