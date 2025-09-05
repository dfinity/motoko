import Prim "mo:prim";

persistent actor {
    persistent func requiredFunction() {
        Prim.debugPrint("REQUIRED FUNCTION");
    };

    persistent func optionalFunction() {
        Prim.debugPrint("OPTIONAL FUNCTION");
    };

    let required = requiredFunction;
    let optional = optionalFunction;
    required();
    optional();
}
