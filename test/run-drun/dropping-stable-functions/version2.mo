import Prim "mo:prim";

persistent actor {
    func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 2");
    };

    var retained = requiredFunction;
    retained();
}
