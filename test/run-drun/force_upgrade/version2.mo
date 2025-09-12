import Prim "mo:prim";

persistent actor {
    persistent func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 2");
    };

    // Error on `__motoko_force_upgrade` skipping persistent function gc.
    // optionFunction is missing

    let retained = requiredFunction;
    retained();
}
