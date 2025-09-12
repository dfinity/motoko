import Prim "mo:prim";

persistent actor {
    persistent func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 0");
    };

    let required = requiredFunction;
    required();
}
