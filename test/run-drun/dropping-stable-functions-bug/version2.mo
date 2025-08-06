import Prim "mo:prim";

persistent actor {
    func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 2");
    };

    let retained = requiredFunction;
    retained();
}
