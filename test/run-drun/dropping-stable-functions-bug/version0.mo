import Prim "mo:prim";

persistent actor {

    func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION VERSION 0");
    };

    func optionalFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION VERSION 0");
    };

    let required = requiredFunction;
    let optional = optionalFunction;
    required();
    optional();
}
