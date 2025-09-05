import Prim "mo:prim";

persistent actor {
    persistent func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 2");
    };

    let retained = requiredFunction;
    retained();
}
