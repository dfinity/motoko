import Prim "mo:prim";

persistent actor {
    stable func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 2");
    };

    let retained = requiredFunction;
    retained();
}
